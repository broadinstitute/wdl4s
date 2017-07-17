package wdl4s.cwl

import shapeless.{:+:, CNil, Witness}
import eu.timepit.refined._
import CwlVersion._
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand, Inputs}


sealed trait Cwl {

  val cwlVersion: Option[CwlVersion]
}

case class Workflow(
  cwlVersion: Option[CwlVersion] = None,
  `class`: Witness.`"Workflow"`.T,
  inputs: WorkflowInput,
  outputs: WorkflowOutput,
  steps: WorkflowSteps) extends Cwl

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
                            inputs: Inputs,
                            outputs:
                              Array[CommandOutputParameter] :+:
                              Map[CommandOutputParameter#Id, CommandOutputParameter#`type`] :+:
                              Map[CommandOutputParameter#Id, CommandOutputParameter] :+:
                              CNil,
                            `class`: W.`"CommandLineTool"`.T,
                            id: Option[String] = None,
                            requirements: Option[Array[Requirement]] = None,
                            hints: Option[Array[String]] = None, //TODO: Any?
                            label: Option[String] = None,
                            doc: Option[String] = None,
                            cwlVersion: Option[CwlVersion] = None,
                            baseCommand: Option[BaseCommand] = None,
                            arguments: Option[Array[Argument]] = None,
                            stdin: Option[ECMAScriptExpression :+: String :+: CNil] = None,
                            stderr: Option[ECMAScriptExpression :+: String :+: CNil] = None,
                            stdout: Option[ECMAScriptExpression :+: String :+: CNil] = None,
                            successCodes: Option[Array[Int]] = None,
                            temporaryFailCodes: Option[Array[Int]] = None,
                            permanentFailCodes: Option[Array[Int]] = None) extends Cwl

object CommandLineTool {
  type Inputs =
    Array[CommandInputParameter] :+: Map[CommandInputParameter#Id, CommandInputParameter#`type`] :+:
      Map[CommandInputParameter#Id, CommandInputParameter] :+: CNil
  type Outputs =
    Array[CommandOutputParameter] :+: Map[CommandOutputParameter#Id, CommandOutputParameter#`type`] :+:
      Map[CommandOutputParameter#Id, CommandOutputParameter] :+: CNil
  type BaseCommand = String :+: Array[String] :+: CNil
  type Argument = ECMAScriptExpression :+: CommandLineBinding :+: String :+: CNil
  type StdChannel = ECMAScriptExpression :+: String :+: CNil
}
