package wdl4s.cwl

import shapeless.{:+:, CNil, Poly1}
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.map._
import ScatterMethod._
import wdl4s.cwl.CwlType.CwlType
import wdl4s.cwl.WorkflowStep.{Inputs, Outputs, Run}
import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wdl.command.CommandPart
import wdl4s.wdl.types.WdlType
import wdl4s.wom.callable.Callable.{InputDefinition, OutputDefinition, RequiredInputDefinition}
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.{Expression, PlaceholderExpression}
import wdl4s.wom.graph.{CallNode, GraphNode}
import shapeless.{:+:, CNil, Coproduct}
import ScatterMethod._
import wdl4s.cwl.WorkflowStep.{Outputs, Run}

/**
  * An individual job to run.
  *
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStep">CWL Spec | Workflow Step</a>
  *
  * @param id
  * @param in
  * @param out
  * @param run Purposefully not defaulted as it's required and it is unreasonable to not have something to run.
  * @param requirements
  * @param hints
  * @param label
  * @param doc
  * @param scatter
  * @param scatterMethod
  */
case class WorkflowStep(
  id: String, //not actually optional but can be declared as a key for this whole object for convenience
  in: Array[WorkflowStepInput] = Array.empty,
  out: Outputs,
  run: Run,
  requirements: Option[Array[Requirement]],
  hints: Option[Array[String]], //TODO: should be 'Any' type
  label: Option[String],
  doc: Option[String],
  scatter: Option[String :+: Array[String] :+: CNil],
  scatterMethod: Option[ScatterMethod]) {

  def womGraphInputNodes: Set[GraphNode] = ???

  def womCallNode: GraphNode = ???

  def taskDefinitionInputs(workflowInputs: Array[InputParameter], otherStepsOutputs: Array[WorkflowStepOutput]):  Set[_ <: Callable.InputDefinition] = ???


  object RunToOutputDefinition extends Poly1 {
    implicit def commandLineTool = at[CommandLineTool] { clt =>
      clt.outputs.toList.foldLeft(Map.empty[String,WdlType]){
         (acc, out) =>
               acc ++
                 out.`type`.flatMap(_.select[CwlType]).map(cwlTypeToWdlType).map(
                   out.id -> _
                 ).toList.toMap
            }
    }
    implicit def string = at[String] { _ => Map.empty[String, WdlType]}
    implicit def expressionTool = at[ExpressionTool] { _ => Map.empty[String, WdlType]}
    implicit def workflow = at[Workflow] { _ => Map.empty[String, WdlType]}
  }

  def taskDefinitionOutputs(): Set[Callable.OutputDefinition] = {
    val typeMap: Map[String, WdlType] = run.fold(RunToOutputDefinition)


      out.select[Array[WorkflowStepOutput]].
      map(_.toList.map(output => OutputDefinition(output.id, typeMap(output.id), PlaceholderExpression(typeMap(output.id))))).
      toSet

    ???
  }

  //def graphNodes = womGraphInputNodes + womCallNode

  def taskDefinition: TaskDefinition = {

    val id = this.id

    val commandTemplate: Seq[CommandPart] = baseCommand.get.fold(BaseCommandPoly)

    val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression])

    val meta: Map[String, String] = Map.empty
    val parameterMeta: Map[String, String] = Map.empty

    /*
    val outputs: Set[Callable.OutputDefinition] = this.out.select[Array[CommandOutputParameter]].toArray.flatten.map {
      output =>
        val tpe = output.`type`.select[CwlType].map(cwlTypeToWdlType).get
        OutputDefinition(output.id, tpe, PlaceholderExpression(tpe))
    }.toSet

    val inputs: Set[_ <: Callable.InputDefinition] =
      this.in.select[Map[WorkflowStepInputId, WorkflowStepInputSource]].map(_.map{
        case (id, source) =>
          //val tpe = cip.`type`.get.select[CwlType].map(cwlTypeToWdlType).get
          RequiredInputDefinition(id, tpe)
      }).get.toSet
      */

    val declarations: List[(String, Expression)] = List.empty

    TaskDefinition(
      id, //this should be non-optional as a type
      commandTemplate,
      runtimeAttributes,
      meta,
      parameterMeta,
      outputs,
      inputs,
      declarations
    )
  }

  def graphNodes: Set[GraphNode] = {
    val cwi = CallNode.callWithInputs(id.getOrElse("this is a made up call node name"), taskDefinition, Map.empty)

    Set.empty[GraphNode] ++ cwi.inputs + cwi.call
  }

  /*
  private def buildWomNodeAndInputs(wdlCall: WdlCall): CallWithInputs = {
    val inputToOutputPort: Map[String, GraphNodeOutputPort] = for {
      (inputName, expr) <- wdlCall.inputMappings
      variable <- expr.variableReferences
      parent <- wdlCall.parent
      node <- parent.resolveVariable(variable.terminal.sourceString)
      outputPort = outputPortFromNode(node, variable.terminalSubIdentifier)
    } yield inputName -> outputPort

    CallNode.callWithInputs(wdlCall.alias.getOrElse(wdlCall.callable.unqualifiedName), wdlCall.callable.womDefinition, inputToOutputPort)
  }


   */
  def womDefinition: TaskDefinition =  {
    val commandTemplate : Seq[CommandPart] = WorkflowStep.runToCommandTemplate(run)
    val runtimeAttributes: RuntimeAttributes = ??? //requirements.

    val meta: Map[String, String] = ???
    val parameterMeta: Map[String, String] = ???
    val outputs: Set[Callable.OutputDefinition] = ???
    val inputs: Set[_ <: Callable.InputDefinition] = ???
    val declarations: List[(String, Expression)] = ???

    TaskDefinition(
      id.get, //this should be non-optional as a type
      commandTemplate,
      runtimeAttributes,
      meta,
      parameterMeta,
      outputs,
      inputs,
      declarations
    )
  }


  /*
  private def buildWomTaskDefinition: TaskDefinition =
  TaskDefinition(
    name,
    commandTemplate,
    runtimeAttributes,
    meta,
    parameterMeta,
    outputs.map(_.womOutputDefinition).toSet,
    buildWomInputs,
    buildWomDeclarations
  )
  */
}

/**
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStepOutput">WorkflowstepOutput</a>
  *
  * @param id
  */
case class WorkflowStepOutput(id: String)

object WorkflowStep {

  def runToCommandTemplate: Run => Seq[CommandPart] = ???

  def fromInputs: Inputs => InputDefinition = ???

  def fromResourceRequirement: ResourceRequirement => RuntimeAttributes = ???

  type Run =
    String :+:
      CommandLineTool :+:
      ExpressionTool :+:
      Workflow :+:
      CNil

  type Inputs =
    Array[WorkflowStepInput] :+:
      Map[WorkflowStepInputId, WorkflowStepInputSource] :+:
      Map[WorkflowStepInputId, WorkflowStepInput] :+:
      CNil

  type Outputs =
    Array[String] :+:
      Array[WorkflowStepOutput] :+:
      CNil

}
