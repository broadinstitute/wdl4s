package wdl4s.cwl

import shapeless.{:+:, CNil, Witness}
import eu.timepit.refined._
import CwlVersion._
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand, Inputs, StdChannel}
import shapeless.{:+:, CNil, Coproduct, Poly1, Witness}
import CwlType._
import shapeless.syntax.singleton._
import eu.timepit.refined._
import CwlVersion._
import cats.data.Validated.Valid
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.cwl.CommandLineTool.{apply => _, _}
import wdl4s.cwl.CwlType.CwlType
import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wdl.command.{CommandPart, StringCommandPart}
import wdl4s.wdl.types._
import wdl4s.wom.callable.Callable.{OutputDefinition, RequiredInputDefinition}
import wdl4s.wom.callable.{Callable, TaskDefinition, WorkflowDefinition}
import wdl4s.wom.executable.Executable
import wdl4s.wom.expression.{Expression, PlaceholderExpression}
import wdl4s.wom.graph.{CallNode, Graph, GraphNode}


sealed trait Cwl {

  val cwlVersion: Option[CwlVersion]
}

case class Workflow(
  cwlVersion: Option[CwlVersion] = None,
  `class`: Witness.`"Workflow"`.T,
  inputs: WorkflowInput,
  outputs: WorkflowOutput,
  steps: WorkflowSteps) extends Cwl {

  def womExecutable: Executable = Executable(womDefinition)

  def womGraph: Graph = ???
  /*
    steps.foldLeft(Set.empty[GraphNode]){
    case (graphNodes, step) => graphNodes ++ step.womGraphInputNodes ++ step.womNode
  }

   def buildWomGraph(wdlWorkflow: WdlWorkflow): Graph = {
    val graphNodes = wdlWorkflow.calls.foldLeft(Set.empty[GraphNode])({
      case (currentNodes, call) => currentNodes ++ call.womGraphInputNodes + call.womCallNode
    })

    Graph.validateAndConstruct(graphNodes) match {
      case Valid(wg) => wg.withDefaultOutputs
      case Invalid(errors) => throw ValidationException("Unable to validate graph", errors.map(new Exception(_)).toList)
    }
  }

  */
  def womDefinition: WorkflowDefinition = {
    val name:String = ???
    val meta: Map[String, String] = ???
    val paramMeta: Map[String, String] = ???
    val declarations: List[(String, Expression)] = ???

    WorkflowDefinition(
      name,
      womGraph,
      meta,
      paramMeta,
      declarations
    )
  }

}

/**
  *
  * @param inputs
  * @param outputs
  * @param `class` This _should_ always be "CommandLineTool," however the spec does not -er- specify this.
  * @param id
  * @param requirements
  * @param hints
  * @param label
  * @param doc
  * @param cwlVersion
  * @param baseCommand
  * @param arguments
  * @param stdin
  * @param stderr
  * @param stdout
  * @param successCodes
  * @param temporaryFailCodes
  * @param permanentFailCodes
  */
case class CommandLineTool(
                            inputs: Inputs = Coproduct[Inputs](Map.empty[String,CommandInputParameter]),
                            outputs: Outputs = Coproduct[Outputs](Array.empty[CommandOutputParameter]),
                            `class`: W.`"CommandLineTool"`.T = "CommandLineTool".narrow,
                            id: Option[String] = None,
                            requirements: Option[Array[Requirement]] = None,
                            hints: Option[Array[String]] = None, //TODO: Any?
                            label: Option[String] = None,
                            doc: Option[String] = None,
                            cwlVersion: Option[CwlVersion] = None,
                            baseCommand: Option[BaseCommand] = None,
                            arguments: Option[Array[Argument]] = None,
                            stdin: Option[StringOrExpression] = None,
                            stderr: Option[StringOrExpression] = None,
                            stdout: Option[StringOrExpression] = None,
                            successCodes: Option[Array[Int]] = None,
                            temporaryFailCodes: Option[Array[Int]] = None,
                            permanentFailCodes: Option[Array[Int]] = None) extends Cwl {

  def womExecutable: ErrorOr[Executable] = {
    Valid(Executable(taskDefinition))
    //Graph.validateAndConstruct(womNode).map(Executable.apply)

  }
  /*
    Graph.
      validateAndConstruct(womNode).
      map(graph =>  WorkflowDefinition("some workflow name", graph, Map.empty, Map.empty, List.empty)).
      map(Executable(_))
      */



  def cwlTypeToWdlType : CwlType => WdlType = {
    case Null => ???
    case Boolean => WdlBooleanType
    case Int => WdlIntegerType
    case Long => WdlIntegerType
    case Float => WdlFloatType
    case Double => WdlFloatType
    case String => WdlStringType
    case CwlType.File => ???
    case CwlType.Directory => ???
  }


  object BaseCommandPoly extends Poly1 {
    implicit def one = at[String] {s => Seq(StringCommandPart(s))}

    implicit def many = at[Array[String]] {_.toSeq.map(StringCommandPart.apply)}
  }

  def taskDefinition: TaskDefinition = {

    val id = this.id.getOrElse("Made this ID up")

    val commandTemplate: Seq[CommandPart] = baseCommand.get.fold(BaseCommandPoly)

    val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression])

    val meta: Map[String, String] = Map.empty
    val parameterMeta: Map[String, String] = Map.empty

    val outputs: Set[Callable.OutputDefinition] = this.outputs.select[Array[CommandOutputParameter]].toArray.flatten.map {
      output =>
        val tpe = output.`type`.select[CwlType].map(cwlTypeToWdlType).get
        OutputDefinition(output.id, tpe, PlaceholderExpression(tpe))
    }.toSet

    val inputs: Set[_ <: Callable.InputDefinition] =
      this.inputs.select[Map[String, CommandInputParameter]].map(_.map{
        case (id, cip) =>
          val tpe = cip.`type`.get.select[CwlType].map(cwlTypeToWdlType).get
          RequiredInputDefinition(id, tpe)
      }).get.toSet

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

  def womNode: Set[GraphNode] = {
    val cwi = CallNode.callWithInputs(id.getOrElse("this is a made up call node name"), taskDefinition, Map.empty)

    println(cwi.inputs)

    Set.empty[GraphNode] ++ cwi.inputs + cwi.call
  }
}

object CommandLineTool {
  type Inputs =
    CommandInputParameter :+: Map[CommandInputParameter#Id, CommandInputParameter#`type`] :+:
      Map[CommandInputParameter#Id, CommandInputParameter] :+: CNil
  type Outputs =
    Array[CommandOutputParameter] :+: Map[CommandOutputParameter#Id, CommandOutputParameter#`type`] :+:
      Map[CommandOutputParameter#Id, CommandOutputParameter] :+: CNil
  type BaseCommand = String :+: Array[String] :+: CNil
  type Argument = ECMAScriptExpression :+: CommandLineBinding :+: String :+: CNil
  type StdChannel = ECMAScriptExpression :+: String :+: CNil
  type StringOrExpression = ECMAScriptExpression :+: String :+: CNil
}
