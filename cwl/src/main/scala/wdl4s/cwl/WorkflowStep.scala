package wdl4s.cwl

import cats.data.NonEmptyList
import cats.syntax.either._
import lenthall.Checked
import lenthall.validation.ErrorOr.ErrorOr
import shapeless._
import wdl4s.cwl.ScatterMethod._
import wdl4s.cwl.WorkflowStep.{Outputs, Run, WorkflowStepInputFold, _}
import wdl4s.wdl.types.WdlAnyType
import wdl4s.wdl.values.WdlValue
import wdl4s.wom.callable.Callable
import wdl4s.wom.callable.Callable._
import wdl4s.wom.expression.PlaceholderWomExpression
import wdl4s.wom.graph.CallNode.{InputDefinitionFold, InputDefinitionPointer}
import wdl4s.wom.graph.GraphNodePort.{GraphNodeOutputPort, OutputPort}
import wdl4s.wom.graph._

import scala.language.postfixOps
import scala.util.Try

/**
  * An individual job to run.
  *
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStep">CWL Spec | Workflow Step</a>
  * @param run Purposefully not defaulted as it's required in the specification and it is unreasonable to not have something to run.
  */
case class WorkflowStep(
                         id: String,
                         in: Array[WorkflowStepInput] = Array.empty,
                         out: Outputs,
                         run: Run,
                         requirements: Option[Array[Requirement]] = None,
                         hints: Option[Array[CwlAny]] = None,
                         label: Option[String] = None,
                         doc: Option[String] = None,
                         scatter: Option[String :+: Array[String] :+: CNil] = None,
                         scatterMethod: Option[ScatterMethod] = None) {


  def typedOutputs: WdlTypeMap = run.fold(RunOutputsToTypeMap)

  def fileName: Option[String] = run.select[String]

  val unqualifiedStepId = Try(WorkflowStepId(id)).map(_.stepId).getOrElse(id)

  /**
    * Generates all GraphNodes necessary to represent call nodes and input nodes
    * Recursive because dependencies are discovered as we iterate through inputs and corresponding
    * upstream nodes need to be generated on the fly.
    */
  def callWithInputs(typeMap: WdlTypeMap,
                     workflow: Workflow,
                     knownNodes: Set[GraphNode],
                     workflowInputs: Map[String, GraphNodeOutputPort]): Checked[Set[GraphNode]] = {

    // To avoid duplicating nodes, return immediately if we've already covered this node
    val haveWeSeenThisStep: Boolean = knownNodes.collect { case TaskCallNode(name, _, _, _) => name }.contains(unqualifiedStepId)

    if (haveWeSeenThisStep) Right(knownNodes)
    else {
      // Create a task definition for the underlying run.
      // For sub workflows, we'll need to handle the case where this could be a workflow definition
      //TODO: turn this select into a fold that supports other types of runnables
      val taskDefinition = run.select[CommandLineTool].map { _.taskDefinition } get

      val callNodeBuilder = new CallNode.CallNodeBuilder()
      
      /**
        * Method used to fold over the list of inputs declared by this step.
        * Note that because we work on saladed CWL, all ids are fully qualified at this point (e.g: file:///path/to/file/r.cwl#cgrep/pattern
        * The goal of this method is two fold (pardon the pun):
        *   1) link each input of the step to an output port (which at this point can be from a different step or from a workflow input)
        *   2) accumulate the nodes created along the way to achieve 1)
        */
      def foldInputs(currentFold: Checked[WorkflowStepInputFold], workflowStepInput: WorkflowStepInput): Checked[WorkflowStepInputFold] = currentFold flatMap {
        fold =>
          // The source from which we expect to satisfy this input (output from other step or workflow input)
          // TODO: this can be None, a single source, or multiple sources. Currently assuming it's a single one
          val inputSource: String = workflowStepInput.source.flatMap(_.select[String]).get

          // Name of the step input
          val stepInputName = WorkflowStepInputOrOutputId(workflowStepInput.id).ioId
          
          val accumulatedNodes = fold.generatedNodes ++ knownNodes
          
          /*
            * Try to find in the given set an output port named stepOutputId in a call node named stepId
            * This is useful when we've determined that the input points to an output of a different step and we want
            * to get the corresponding output port.
           */
          def findThisInputInSet(set: Set[GraphNode], stepId: String, stepOutputId: String): Checked[OutputPort] = {
            for {
            // We only care for outputPorts of call nodes
              call <- set.collectFirst { case callNode: CallNode if callNode.name == stepId => callNode }.
                toRight(NonEmptyList.one(s"stepId $stepId not found in known Nodes $set"))
              output <- call.outputPorts.find(_.name == stepOutputId).
                toRight(NonEmptyList.one(s"step output id $stepOutputId not found in ${call.outputPorts}"))
            } yield output
          }

          /*
            * Build a wom node for the given step and return the newly created nodes
            * This is useful when we've determined that the input belongs to an upstream step that we haven't covered yet
           */
          def buildUpstreamNodes(upstreamStepId: String): Checked[Set[GraphNode]] =
          // Find the step corresponding to this upstreamStepId in the set of all the steps of this workflow
            for {
              step <- workflow.steps.find { step => WorkflowStepId(step.id).stepId == upstreamStepId }.
                toRight(NonEmptyList.one(s"no step of id $upstreamStepId found in ${workflow.steps.map(_.id)}"))
              call <- step.callWithInputs(typeMap, workflow, accumulatedNodes, workflowInputs)
            } yield call

          def fromWorkflowInput: Checked[WorkflowStepInputFold] = {
            // Try to find it in the workflow inputs map, if we can't it's an error
            workflowInputs.collectFirst {
              case (inputId, port) if inputSource == inputId => updateFold(port)
            } getOrElse {
              Left(NonEmptyList.one(s"Can't find workflow input for $inputSource"))
            }
          }

          def fromStepOutput(stepId: String, stepOutputId: String): Checked[WorkflowStepInputFold] = {
            // First check if we've already built the WOM node for this step, and if so return the associated output port
            findThisInputInSet(accumulatedNodes, stepId, stepOutputId).flatMap(updateFold(_))
              .orElse {
                // Otherwise build the upstream nodes and look again in those newly created nodes
                for {
                  newNodes <- buildUpstreamNodes(stepId)
                  outputPort <- findThisInputInSet(newNodes, stepId, stepOutputId)
                  newFold <- updateFold(outputPort, newNodes)
                } yield newFold
              }
          }

          def updateFold(outputPort: OutputPort, newCallNodes: Set[GraphNode] = Set.empty): Checked[WorkflowStepInputFold] = {
            // TODO for now we only handle a single input source, but there may be several
            workflowStepInput.toExpressionNode(Map(inputSource -> outputPort)).map({ expressionNode =>
              fold
                .withMapping(stepInputName, expressionNode)
                .withNewNodes(newCallNodes + expressionNode)
            }).toEither
          }

          /*
            * Parse the inputSource (what this input is pointing to)
            * 2 cases:
            *   - points to a workflow input
            *   - points to an upstream step
           */
          FullyQualifiedName(inputSource) match {
            // The source points to a workflow input, which means it should be in the workflowInputs map
            case _: WorkflowInputId => fromWorkflowInput
            // The source points to an output from a different step
            case WorkflowStepInputOrOutputId(_, stepId, stepOutputId) => fromStepOutput(stepId, stepOutputId)
          }
      }

     /*
       * Folds over input definitions and build an InputDefinitionFold
      */
      def foldInputDefinitions(expressionNodes: Map[String, ExpressionNode], callable: Callable): Checked[InputDefinitionFold] = {

        def foldFunction(currentFold: Checked[InputDefinitionFold], inputDefinition: InputDefinition): Checked[InputDefinitionFold] = currentFold flatMap {
          fold =>
            inputDefinition match {
                // We got an expression node, meaning there was a workflow step input for this input definition
                // Add the mapping, create an input port from the expression node and add the expression node to the fold
              case _ if expressionNodes.contains(inputDefinition.name) =>
                val expressionNode = expressionNodes(inputDefinition.name)
                fold
                  .withMapping(inputDefinition, expressionNode.inputDefinitionPointer)
                  .withInputPort(callNodeBuilder.makeInputPort(inputDefinition, expressionNode.singleExpressionOutputPort))
                  .withExpressionNode(expressionNode)  
                  .asRight[NonEmptyList[String]]

                // No expression node mapping, use the default
              case withDefault @ InputDefinitionWithDefault(_, _, expression) =>
                fold
                  .withMapping(withDefault, Coproduct[InputDefinitionPointer](expression))
                  .asRight[NonEmptyList[String]]

                // Required input without default value and without mapping, this is a validation error
              case RequiredInputDefinition(requiredName, _) =>
                NonEmptyList.one(s"Input $requiredName is required and is not bound to any value")
                  .asLeft[InputDefinitionFold]

                // Optional input without mapping, defaults to empty value
              case optional: OptionalInputDefinition =>
                fold
                  .withMapping(optional, Coproduct[InputDefinitionPointer](optional.womType.none: WdlValue))
                  .asRight[NonEmptyList[String]]
            }
        }
        callable.inputs.foldLeft(InputDefinitionFold.empty.asRight[NonEmptyList[String]])(foldFunction)
      }

      // Use what we've got to generate a call node and required input nodes
      // However here we expect WOM NOT to return any required input nodes because it would mean that some task definition inputs
      // have not been linked to either a workflow input or an upstream output, in which case they have no other way to be satisfied ( <- is that true ?)
      for {
        stepInputFold <- in.foldLeft(WorkflowStepInputFold.empty.asRight[NonEmptyList[String]])(foldInputs)
        inputDefinitionFold <- foldInputDefinitions(stepInputFold.stepInputMapping, taskDefinition)
        callAndNodes = callNodeBuilder.build(unqualifiedStepId, taskDefinition, inputDefinitionFold)
      } yield stepInputFold.generatedNodes ++ callAndNodes.nodes ++ knownNodes
    }
  }
}

/**
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStepOutput">WorkflowstepOutput</a>
  */
case class WorkflowStepOutput(id: String)

object WorkflowStep {

  private [cwl] object WorkflowStepInputFold {
    private [cwl] def empty = WorkflowStepInputFold(Map.empty, Set.empty)
  }
  private [cwl] case class WorkflowStepInputFold(stepInputMapping: Map[String, ExpressionNode], generatedNodes: Set[GraphNode]) {
    def withMapping(stepInput: String, value: ExpressionNode) = this.copy(stepInputMapping = stepInputMapping + (stepInput -> value))
    def withNewNodes(newNodes: Set[GraphNode]) = this.copy(generatedNodes = generatedNodes ++ newNodes)
    def withNewNode(newNode: GraphNode) = this.copy(generatedNodes = generatedNodes + newNode)
  }

  val emptyOutputs: Outputs = Coproduct[Outputs](Array.empty[String])

  implicit class EnhancedWorkflowStepInput(val workflowStepInput: WorkflowStepInput) extends AnyVal {
    def toExpressionNode(sourceMappings: Map[String, OutputPort]): ErrorOr[ExpressionNode] = {
      val womExpression = PlaceholderWomExpression(sourceMappings.keySet, WdlAnyType)
      ExpressionNode.linkWithInputs(workflowStepInput.id, womExpression, sourceMappings)
    }
  }

  object InputSourcesFold extends Poly1 {
    implicit def one = at[String] { Set(_): Set[String] }
    implicit def many = at[Array[String]] { _.toSet: Set[String] }
  }

  type Run =
    String :+:
      CommandLineTool :+:
      ExpressionTool :+:
      Workflow :+:
      CNil

  type Outputs =
    Array[String] :+:
      Array[WorkflowStepOutput] :+:
      CNil
}
