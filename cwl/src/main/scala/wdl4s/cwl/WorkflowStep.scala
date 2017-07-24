package wdl4s.cwl

import shapeless.{:+:, CNil}
import ScatterMethod._
import wdl4s.cwl.WorkflowStep.{Inputs, Outputs, Run}
import wdl4s.wdl.RuntimeAttributes
import wdl4s.wdl.command.CommandPart
import wdl4s.wom.callable.Callable.InputDefinition
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.Expression
import wdl4s.wom.graph.GraphNode

case class WorkflowStep(
  id: Option[String], //not actually optional but can be declared as a key for this whole object for convenience
  in: 
    Array[WorkflowStepInput] :+:
    Map[WorkflowStepInputId, WorkflowStepInputSource] :+:
    Map[WorkflowStepInputId, WorkflowStepInput] :+:
    CNil,
  out: 
    Array[String] :+:
    Array[WorkflowStepOutput] :+:
    CNil,
  run: 
    String :+:
    CommandLineTool :+:
    ExpressionTool :+:
    Workflow :+:
    CNil,
  requirements: Option[Array[Requirement]],
  hints: Option[Array[String]], //TODO: should be 'Any' type
  label: Option[String],
  doc: Option[String],
  scatter: Option[String :+: Array[String] :+: CNil],
  scatterMethod: Option[ScatterMethod]) {

  def womGraphInputNodes: Set[GraphNode] = ???

  def womCallNode: GraphNode = ???

  def graphNodes = womGraphInputNodes + womCallNode

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

