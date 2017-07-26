package wdl4s.cwl

import shapeless.{:+:, CNil, Poly1}
import cats.syntax.foldable._
import cats.instances.list._
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

case class WorkflowStep(
  id: String, //not actually optional but can be declared as a key for this whole object for convenience
  in: Inputs,
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

  def taskDefinitionInputs(workflowInputs: WorkflowInput, otherStepsOutputs: Array[WorkflowStepOutput]):  Set[_ <: Callable.InputDefinition] = ???

  object CommandLineToolOutputTypes extends Poly1 {
    implicit def mapTo = at[Map[CommandOutputParameter#Id, CommandOutputParameter]](_.map {
      case (id, commandOutputParameter) => id -> commandOutputParameter.`type`.select[CwlType].map(cwlTypeToWdlType).get
    })

    implicit def array = at[Array[CommandOutputParameter]] {_ => Map.empty[String, WdlType]}

    implicit def typeMap = at[Map[CommandOutputParameter#Id, CommandOutputParameter#`type`]] {_ => Map.empty[String, WdlType]}
  }

  object RunToOutputDefinition extends Poly1 {
    implicit def commandLineTool = at[CommandLineTool] {_.outputs.fold(CommandLineToolOutputTypes)}
    implicit def string = at[String] { _ => Map.empty[String, WdlType]}
    implicit def expressionTool = at[ExpressionTool] { _ => Map.empty[String, WdlType]}
    implicit def workflow = at[Workflow] { _ => Map.empty[String, WdlType]}
  }

  def taskDefinitionOutputs(): Set[Callable.OutputDefinition] = {
    val typeMap = run.fold(RunToOutputDefinition)

    out.
      select[Array[WorkflowStepOutput]].
      map(_.toList.map(output => OutputDefinition(output.id, typeMap(output.id), PlaceholderExpression(typeMap(output.id))))).
      toSet
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

