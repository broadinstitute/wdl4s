package wdl4s.cwl

import shapeless.{:+:, CNil, Poly1}
import mouse.all._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.map._
import ScatterMethod._
import wdl4s.cwl.CwlType.CwlType
import wdl4s.cwl.WorkflowStep.{Inputs, Outputs, Run}
import wdl4s.wdl.{FullyQualifiedName, RuntimeAttributes, WdlExpression}
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
  requirements: Option[Array[Requirement]] = None,
  hints: Option[Array[CwlAny]] = None,
  label: Option[String] = None,
  doc: Option[String] = None,
  scatter: Option[String :+: Array[String] :+: CNil] = None,
  scatterMethod: Option[ScatterMethod] = None) {

  def womGraphInputNodes: Set[GraphNode] = ???

  def womCallNode: GraphNode = ???

  def taskDefinitionInputs(typeMap: TypeMap):  Set[_ <: Callable.InputDefinition] =
    in.map{wsi =>
      val tpe = typeMap(wsi.source.flatMap(_.select[String]).get)
      RequiredInputDefinition(wsi.id, tpe)
    }.toSet



  object OutputsToTypeMap extends Poly1 {

    def fullIdToOutputDefintition(fullyQualifiedName: String, typeMap: TypeMap) = {

      //we want to only look at the id, not the filename
      val _id = fullyQualifiedName.substring(fullyQualifiedName.lastIndexOf("/") + 1,fullyQualifiedName.length())

      OutputDefinition(_id, typeMap(_id), PlaceholderExpression(typeMap(_id)))
    }

    implicit def a = at[Array[WorkflowStepOutput]] { o =>
      (typeMap: TypeMap) =>
        o.map(output => fullIdToOutputDefintition(output.id, typeMap)).toSet
    }

    implicit def b = at[Array[String]] { o =>
      (typeMap: TypeMap) =>
        o.map(fullIdToOutputDefintition(_, typeMap)).toSet
    }

  }

  def taskDefinitionOutputs(cwlMap: Map[String, Cwl]): Set[Callable.OutputDefinition] = {

    println(s"id is $id")

    val runnableFQNTypeMap: TypeMap = run.fold(RunToTypeMap).apply(cwlMap)

    val runnableIdToTypeMap = runnableFQNTypeMap.map {
      case (id, tpe) => id.substring(id.lastIndexOf("#") + 1,id.length()) -> tpe
    }.map {
      case (runnableId, tpe) => (runnableId + id.substring(id.lastIndexOf("#") + 1,id.lastIndexOf("/"))) -> tpe
    }

    out.fold(OutputsToTypeMap).apply(runnableIdToTypeMap)
  }

  def taskDefinition(typeMap: TypeMap, cwlMap: Map[String, Cwl]): TaskDefinition = {

    val id = this.id

    val commandTemplate: Seq[CommandPart] = run.select[CommandLineTool].map(_.baseCommand.get.fold(BaseCommandToCommandParts)).toSeq.flatten

    val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression])

    val meta: Map[String, String] = Map.empty
    val parameterMeta: Map[String, String] = Map.empty

    val declarations: List[(String, Expression)] = List.empty

    TaskDefinition(
      id, //this should be non-optional as a type
      commandTemplate,
      runtimeAttributes,
      meta,
      parameterMeta,
      taskDefinitionOutputs(cwlMap),
      taskDefinitionInputs(typeMap),
      declarations
    )
  }

  def graphNodes(typeMap: TypeMap, cwlMap: Map[String, Cwl]): Set[GraphNode] = {
    val cwi = CallNode.callWithInputs(id, taskDefinition(typeMap, cwlMap), Map.empty)

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
