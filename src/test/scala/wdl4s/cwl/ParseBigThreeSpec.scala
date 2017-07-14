package wdl4s.cwl


import io.circe.syntax._
import io.circe._
import io.circe.generic.extras.auto._
import io.circe.shapes._
import io.circe.generic.auto._
import org.scalatest._
import io.circe.Json
import eu.timepit.refined.api.Refined
import eu.timepit.refined._
import org.scalatest._
import io.circe.yaml.parser

import io.circe.parser._
import io.circe.shapes._
import io.circe.generic.auto._

import io.circe._

import eu.timepit.refined.string._

import io.circe.refined._

class ParseBigThreeSpec extends FlatSpec with Matchers {
  val namespace = "cwl"

  it should "parse 1st tool" in {
  val firstTool = """
cwlVersion: v1.0
class: CommandLineTool
baseCommand: echo
inputs:
  message:
    type: string
    inputBinding:
      position: 1
outputs: []
"""

  decodeCwl(firstTool).isRight shouldBe true
  }

  it should "parse first workflow" in {
    val firstWorkflow = """
cwlVersion: v1.0
class: Workflow
s: Hi
inputs:
  inp: File
  ex: string

outputs:
  classout:
    type: File
    outputSource: compile/classfile

steps:
  untar:
    run: tar-param.cwl
    in:
      tarfile: inp
      extractfile: ex
    out: [example_out]
  compile:
    run: arguments.cwl
    in:
      src: untar/example_out
    out: [classfile]
"""
    decodeCwl(firstWorkflow)
      .isRight should be (true)
  }

  it should "produce coproducts easily" in {
    import shapeless.syntax.inject._
    import shapeless.ops.coproduct.Inject
    val m = new Workflow(
      None,
      `class` = "Workflow",
      inputs = Array.empty[InputParameter].inject[WorkflowInput],
      outputs = Array.empty[WorkflowOutputParameter].inject[WorkflowOutput],
      steps = Array.empty[WorkflowStep].inject[WorkflowSteps])
  }

  it should "parse env cwl" in {
    val envCwl = """
cwlVersion: v1.0
class: CommandLineTool
baseCommand: env
requirements:
  EnvVarRequirement:
    envDef:
      HELLO: $(inputs.message)
inputs:
  message: string
outputs: []
"""

    val output = decodeCwl(envCwl)
    println(output)
    output.isRight should be (true)
  }
}
