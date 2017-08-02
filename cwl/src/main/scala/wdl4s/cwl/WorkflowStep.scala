package wdl4s.cwl

import shapeless.{:+:, CNil}
import wdl4s.cwl.CwlType.CwlType
import wdl4s.cwl.ScatterMethod._
import wdl4s.cwl.WorkflowStep.{Outputs, Run}
import wdl4s.wdl.command.CommandPart
import wdl4s.wdl.types.WdlType
import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wom.callable.Callable.RequiredInputDefinition
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.Expression
import wdl4s.wom.graph.CallNode.CallWithInputs
import wdl4s.wom.graph.GraphNodePort.GraphNodeOutputPort
import wdl4s.wom.graph.{CallNode, GraphNode, RequiredGraphInputNode}

/**
  * An individual job to run.
  *
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStep">CWL Spec | Workflow Step</a>
  * @param run Purposefully not defaulted as it's required and it is unreasonable to not have something to run.
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

  def typedOutputs(cwlMap: Map[String, CwlFile]): WdlTypeMap = run.fold(RunOutputsToTypeMap).apply(cwlMap)

  def womGraphInputNodes: Set[GraphNode] = ???

  def womCallNode: GraphNode = ???

  def taskDefinitionInputs(typeMap: WdlTypeMap): Set[_ <: Callable.InputDefinition] =
    in.map { workflowStepInput =>

      val _source: String = workflowStepInput.source.flatMap(_.select[String]).get

      val mungedTypeMap = typeMap map {
        case (i, t) => RunOutputsToTypeMap.mungeId(i) -> t
      }

      val value = WorkflowStep.mungeInputId(_source)

      val tpe = mungedTypeMap(value)
      RequiredInputDefinition(_source, tpe)
    }.toSet


  def taskDefinitionOutputs(cwlMap: Map[String, CwlFile]): Set[Callable.OutputDefinition] = {

    val runnableFQNTypeMap: WdlTypeMap = run.fold(RunOutputsToTypeMap).apply(cwlMap)

    val runnableIdToTypeMap = runnableFQNTypeMap.map {
      case (i, tpe) => RunOutputsToTypeMap.mungeId(i) -> tpe
    }

    out.fold(WorkflowOutputsToOutputDefinition).apply(runnableIdToTypeMap)
  }

  def taskDefinition(typeMap: WdlTypeMap, cwlMap: Map[String, CwlFile]): TaskDefinition = {

    val id = this.id

    val commandTemplate: Seq[CommandPart] =
    //TODO: turn this select into a fold that supports other types of runnables
      run.select[CommandLineTool].map {
        clt =>
          clt.baseCommand.map(_.fold(BaseCommandToCommandParts)).toSeq.flatten ++
            clt.arguments.map(_.map(_.fold(ArgumentToCommandPart)).toSeq).toSeq.flatten
      }.toSeq.flatten

    val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression])

    val meta: Map[String, String] = Map.empty
    val parameterMeta: Map[String, String] = Map.empty

    val declarations: List[(String, Expression)] = List.empty

    TaskDefinition(
      id,
      commandTemplate,
      runtimeAttributes,
      meta,
      parameterMeta,
      taskDefinitionOutputs(cwlMap),
      taskDefinitionInputs(typeMap),
      declarations
    )
  }

  def callWithInputs(typeMap: WdlTypeMap, cwlMap: Map[String, CwlFile], workflow: Workflow): CallWithInputs = {

    val workflowInputs =
      workflow.inputs.map {
        workflowInput =>
          val tpe = workflowInput.`type`.flatMap(_.select[CwlType]).map(cwlTypeToWdlType).get
          val node = RequiredGraphInputNode(workflowInput.id, tpe)
          workflowInput.id -> GraphNodeOutputPort(workflowInput.id, tpe, node)
      }.toMap


    //need the outputs mapped from other workflow steps in order to pass in this map
    val workflowOutputsMap = in.flatMap {
      workflowStepInput =>

        def lookupStepWithOutput(stepId: String, outputId: String): (WorkflowStep, (String, WdlType)) = {
          val step = workflow.steps.find { step =>
            val lookup = WorkflowStepId(step.id).stepId
            lookup == stepId
          }.get

          step.typedOutputs(cwlMap).map(step -> _)
            .find {
              case (_, (stepOutputId, _)) =>

                RunOutputId(stepOutputId).outputId == outputId
            }.get
        }

        val inputSource = workflowStepInput.source.flatMap(_.select[String]).get

        FullyQualifiedName(inputSource) match {
          case WorkflowStepOutputIdReference(_, stepOutputId, stepId) =>
            val (step, (_, tpe)) = lookupStepWithOutput(stepId, stepOutputId)
            List(inputSource -> GraphNodeOutputPort(workflowStepInput.id, tpe, step.callWithInputs(typeMap, cwlMap, workflow).call))
          case _ => List.empty[(String, GraphNodeOutputPort)]
        }

    }.toMap

    val td = taskDefinition(typeMap, cwlMap)

    CallNode.callWithInputs(id, td, workflowInputs ++ workflowOutputsMap)
  }

}

/**
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStepOutput">WorkflowstepOutput</a>
  */
case class WorkflowStepOutput(id: String)

object WorkflowStep {

  def mungeInputId(in: String): String = {
    val afterHash = in.substring(in.indexOf("#") + 1)

    // Everything after the '/' if the '/' is present, otherwise the whole string.
    afterHash.substring(afterHash.lastIndexOf("/") + 1)
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
