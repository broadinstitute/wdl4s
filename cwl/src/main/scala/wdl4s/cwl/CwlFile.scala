package wdl4s.cwl

import cats.data.Validated.Valid
import cats.instances.list._
import cats.instances.set._
import cats.syntax.foldable._
import lenthall.validation.ErrorOr.ErrorOr
import shapeless.syntax.singleton._
import shapeless.{:+:, CNil, Poly1, Witness}
import wdl4s.cwl.CommandLineTool.{BaseCommand, StringOrExpression}
import wdl4s.cwl.CwlType.CwlType
import wdl4s.cwl.CwlVersion._
import wdl4s.wdl.command.CommandPart
import wdl4s.wdl.types._
import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wom.callable.Callable.{OutputDefinition, RequiredInputDefinition}
import wdl4s.wom.callable.{Callable, TaskDefinition, WorkflowDefinition}
import wdl4s.wom.executable.Executable
import wdl4s.wom.expression.{Expression, PlaceholderExpression}
import wdl4s.wom.graph.GraphNodePort.{GraphNodeOutputPort, OutputPort}
import wdl4s.wom.graph._

sealed trait CwlFile {

  val cwlVersion: Option[CwlVersion]
}

case class Workflow(
                     cwlVersion: Option[CwlVersion] = Option(CwlVersion.Version1),
                     `class`: Workflow.`class`.type = Workflow.`class`,
                     inputs: Array[InputParameter] = Array.empty,
                     outputs: Array[WorkflowOutputParameter] = Array.empty,
                     steps: Array[WorkflowStep]) extends CwlFile {

  def womExecutable(cwlMap: Map[String, CwlFile]): ErrorOr[Executable] = womDefinition(cwlMap).map(Executable.apply)

  def outputsTypeMap(cwlMap: Map[String, CwlFile]): WdlTypeMap = steps.foldLeft(Map.empty[String, WdlType]) {
    (acc, s) => acc ++ s.typedOutputs(cwlMap)
  }

  lazy val stepById: Map[String, WorkflowStep] = steps.map(ws => ws.id -> ws).toMap

  def womGraph(cwlMap: Map[String, CwlFile]): ErrorOr[Graph] = {
    val map: WdlTypeMap =
      outputsTypeMap(cwlMap) ++
        inputs.toList.flatMap { i =>
          i.`type`.flatMap(_.select[CwlType].map(cwlTypeToWdlType).map(i.id -> _)).toList
        }.toMap

    val graphFromSteps = steps.toList.foldMap(_.callWithInputs(map, cwlMap, this).nodes)

    val graphFromInputs: Set[GraphNode] = inputs.map {
      input =>
        input.`type`.flatMap(_.select[CwlType].map(cwlTypeToWdlType)).map(RequiredGraphInputNode(input.id, _)).get
    }.toSet

    val graphFromOutputs: Set[GraphNode] =
      outputs.map {
        output =>
          val tpe = cwlTypeToWdlType(output.`type`.flatMap(_.select[CwlType]).get)

          def lookupOutputSource(source: String): OutputPort = {
            val workflowOutput = WorkflowStepOutputIdReference(source)
            val node = stepById(s"${workflowOutput.fileName}#${workflowOutput.stepId}").callWithInputs(map, cwlMap, this).call

            GraphNodeOutputPort(output.id, tpe, node)
          }

          GraphOutputNode(
            output.id,
            tpe,
            lookupOutputSource(output.outputSource.flatMap(_.select[String]).get)
          )
      }.toSet

    Graph.validateAndConstruct(graphFromSteps ++ graphFromInputs ++ graphFromOutputs)
  }

  def womDefinition(cwlMap: Map[String, CwlFile]): ErrorOr[WorkflowDefinition] = {
    val name: String = "workflow Id"
    val meta: Map[String, String] = Map.empty
    val paramMeta: Map[String, String] = Map.empty
    val declarations: List[(String, Expression)] = List.empty

    womGraph(cwlMap).map(graph =>
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
  val `class`: Witness.`"Workflow"`.T = "Workflow".narrow
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
                            `class`: Witness.`"CommandLineTool"`.T = "CommandLineTool".narrow,
                            id: Option[String] = None,
                            requirements: Option[Array[Requirement]] = None,
                            //hints: Option[Array[CwlAny]] = None,
                            hints: Option[Array[Map[String, String]]] = None,
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
                            permanentFailCodes: Option[Array[Int]] = None) extends CwlFile {

  def womExecutable: ErrorOr[Executable] =
    Valid(Executable(taskDefinition))


  object BaseCommandToString extends Poly1 {
    implicit def one = at[String] {
      identity
    }

    implicit def many = at[Array[String]] {
      _.mkString(" && ")
    }
  }

  object ArgumentToId extends Poly1 {
    implicit def ecmaScript = at[ECMAScriptExpression] {
      _.value
    }

    implicit def commandLineBinding = at[CommandLineBinding] { _ => "" }

    implicit def string = at[String] {
      identity
    }
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

    val commandTemplate: Seq[CommandPart] = baseCommand.get.fold(BaseCommandToCommandParts)

    val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression])

    val meta: Map[String, String] = Map.empty
    val parameterMeta: Map[String, String] = Map.empty

    //TODO: This output does _not_ capture expressions from the output.outputBinding
    //The implementation must include the expression evaluation pieces as detailed in:
    //http://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputBinding
    val outputs: Set[Callable.OutputDefinition] = this.outputs.map {
      output =>
        val tpe = output.`type`.flatMap(_.select[CwlType]).map(cwlTypeToWdlType).get //<-- here be `get` dragons
        OutputDefinition(output.id, tpe, PlaceholderExpression(tpe))
    }.toSet

    val inputs: Set[_ <: Callable.InputDefinition] =
      this.inputs.map { cip =>
        val tpe = cip.`type`.flatMap(_.select[CwlType]).map(cwlTypeToWdlType).get

        //TODO: This id includes the filename, which makes assigning input values more laborious
        //We should consider dropping filenames for _all_ ids, as long as we can guarantee uniqueness
        val inputId = cip.id
        RequiredInputDefinition(inputId, tpe)
      }.toSet

    val declarations: List[(String, Expression)] = List.empty

    TaskDefinition(
      id,
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

    //need to gather up the step outputs and pass them into Call with inputs


    val cwi = CallNode.callWithInputs(id.getOrElse("this is a made up call node name"), taskDefinition, Map.empty)

    Set.empty[GraphNode] ++ cwi.inputs + cwi.call
  }
}

object CommandLineTool {
  val `class`: Witness.`"CommandLineTool"`.T = "CommandLineTool".narrow

  type StringOrExpression = ECMAScriptExpression :+: String :+: CNil

  type BaseCommand = String :+: Array[String] :+: CNil

  type Argument = ECMAScriptExpression :+: CommandLineBinding :+: String :+: CNil
}

