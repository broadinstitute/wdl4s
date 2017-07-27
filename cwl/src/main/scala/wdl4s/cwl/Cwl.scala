package wdl4s.cwl

import shapeless.{:+:, CNil, Coproduct, Witness}
import shapeless.syntax.singleton._
import eu.timepit.refined._
import CwlVersion._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.set._
import shapeless.{:+:, CNil, Coproduct, Poly1, Witness}
import CwlType._
import shapeless.syntax.singleton._
import eu.timepit.refined._
import CwlVersion._
import cats.Show
import cats.data.Validated.Valid
import lenthall.validation.ErrorOr.ErrorOr
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
                     cwlVersion: Option[CwlVersion] = Option(CwlVersion.Version1),
                     `class` : Workflow.`class`.type = Workflow.`class`,
                     inputs: Array[InputParameter] = Array.empty,
                     outputs: Array[WorkflowOutputParameter] = Array.empty,
                     steps: Array[WorkflowStep]) extends Cwl {

  def womExecutable: ErrorOr[Executable] = womDefinition.map(Executable.apply)

  object WorkflowStepsToGraphNodes extends Poly1 {
    implicit def ws = at[Array[WorkflowStep]] {
      _.toList.foldMap {
        _.graphNodes
      }
    }

    //placeholder as we plan to get rid of these
    implicit def map = at[Map[String, WorkflowStep]] { _ => Set.empty[GraphNode] }
  }


  def womGraph: ErrorOr[Graph] =
    Graph.validateAndConstruct(steps.fold(WorkflowStepsToGraphNodes))


  /*
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
  def womDefinition: ErrorOr[WorkflowDefinition] = {
    val name: String = ???
    val meta: Map[String, String] = ???
    val paramMeta: Map[String, String] = ???
    val declarations: List[(String, Expression)] = ???

    womGraph.map(graph =>
      WorkflowDefinition(
        name,
        graph,
        meta,
        paramMeta,
        declarations
      )
    )
  }
}


object Workflow {
  val `class` : Witness.`"Workflow"`.T = "Workflow".narrow
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
                            inputs: Array[CommandInputParameter] = Array.empty,
                            outputs: Array[CommandOutputParameter] = Array.empty,
                            `class`: CommandLineTool.`class`.type = CommandLineTool.`class`,
                            id: Option[String] = None,
                            requirements: Option[Array[Requirement]] = None,
                            hints: Option[Array[String]] = None, //TODO: Any?
                            label: Option[String] = None,
                            doc: Option[String] = None,
                            cwlVersion: Option[CwlVersion] = Option(CwlVersion.Version1),
                            baseCommand: Option[BaseCommand] = None,
                            arguments: Option[Array[CommandLineTool.Argument]] = None,
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




  object BaseCommandPoly extends Poly1 {
    implicit def one = at[String] {s => Seq(StringCommandPart(s))}

    implicit def many = at[Array[String]] {_.toSeq.map(StringCommandPart.apply)}
  }

  object BaseCommandToString extends Poly1 {
    implicit def one = at[String] {identity}

    implicit def many = at[Array[String]] {_.mkString(" && ")}
  }

  object ArgumentToId extends Poly1 {
    implicit def ecmaScript = at[ECMAScriptExpression] {_.value}

    implicit def commandLineBinding = at[CommandLineBinding] {_ => ""}

    implicit def string = at[String] {identity}
  }

  /**
    * This is used in place of the id when id is None.
    *
    * @return
    */
  def taskDefinitionId: String =
    baseCommand.map(_.fold(BaseCommandToString)).getOrElse(
      arguments.map(_.map(_.fold(ArgumentToId)).mkString(" ")).get)

  def taskDefinition: TaskDefinition = {

    val id = this.id.getOrElse(taskDefinitionId)

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

  def graphNodes: Set[GraphNode] = {
    val cwi = CallNode.callWithInputs(id.getOrElse("this is a made up call node name"), taskDefinition, Map.empty)

    Set.empty[GraphNode] ++ cwi.inputs + cwi.call
  }
}

object CommandLineTool {
  val `class` : Witness.`"CommandLineTool"`.T = "CommandLineTool".narrow

  type StringOrExpression = ECMAScriptExpression :+: String :+: CNil

  type BaseCommand = String :+: Array[String] :+: CNil

  type Argument = ECMAScriptExpression :+: CommandLineBinding :+: String :+: CNil
}
