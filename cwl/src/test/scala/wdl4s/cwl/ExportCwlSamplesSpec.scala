package wdl4s.cwl

import io.circe.Printer
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{Coproduct, Witness}
import wdl4s.cwl.CwlVersion.CwlVersion
import wdl4s.cwl.WorkflowStep.{Inputs, Run}

class ExportCwlSamplesSpec extends FlatSpec with Matchers {

  it should "encode sample CWL command line tool" in {
    val tool =
      CommandLineTool(
        inputs = Coproduct[CommandLineTool.Inputs](Map("message" -> CommandInputParameter(
          id = None,
          label = None,
          secondaryFiles = None,
          format = None,
          streamable = None,
          doc = None,
          inputBinding = Option(CommandLineBinding(
            loadContents = None,
            position = Option(1),
            prefix = None,
            separate = None,
            itemSeparator = None,
            valueFrom = None,
            shellQuote = None)),
          default = None,
          `type` = None
        ))),
        outputs = Coproduct[CommandLineTool.Outputs](Array.empty[CommandOutputParameter]),
        `class` = "CommandLineTool",
        id = None,
        requirements = None,
        hints = None,
        label = None,
        doc = None,
        cwlVersion = Option(CwlVersion.Version1),
        baseCommand = None,
        arguments = None,
        stdin = None,
        stderr = None,
        stdout = None,
        successCodes = None,
        temporaryFailCodes = None,
        permanentFailCodes = None)
    val toolJson = encodeCwlCommandLineTool(tool)
    val printer = new Printer(preserveOrder = true, dropNullKeys = true, indent = "  ")
    val toolJsonString = printer.pretty(toolJson)
    val expectedToolJsonString =
      """{"inputs":{"message":{"inputBinding":{"position":1}}},"outputs":[],"class":"CommandLineTool","cwlVersion":"v1.0"}"""
    toolJsonString shouldBe expectedToolJsonString
  }

  it should "encode sample CWL workflow" in {
    val workflow = Workflow(
      cwlVersion = Option(CwlVersion.Version1),
      `class` = "Workflow",
      inputs = Coproduct[WorkflowInput](
        Map(
          "inp" -> Coproduct[MyriadInputType](CwlType.File),
          "ex" -> Coproduct[MyriadInputType](CwlType.String)
        )
      ),
      outputs = Coproduct[WorkflowOutput](
        Map(
          "classout" -> WorkflowOutputParameter(
            id = None,
            `type` = Some(Coproduct[MyriadOutputType](CwlType.File)),
            outputSource = Some(Coproduct[WorkflowOutputParameter#OutputSource]("compile/classfile"))
          )
        )
      ),
      steps = Coproduct[WorkflowSteps](
        Map(
          "untar" -> WorkflowStep(
            id = None,
            run = Coproduct[WorkflowStep.Run]("tar-param.cwl"),
            in = Coproduct[WorkflowStep.Inputs](
              Map(
                "tarfile" -> WorkflowStepInput("inp"),
                "extractfile" -> WorkflowStepInput("ex")
              )
            ),
            out = Coproduct[WorkflowStep.Outputs](Array("example_out"))
          ),
          "compile" -> WorkflowStep(
            id = None,
            run = Coproduct[WorkflowStep.Run]("arguments.cwl"),
            in = Coproduct[WorkflowStep.Inputs](
              Map(
                "src" -> WorkflowStepInput("untar/example_out")
              )
            ),
            out = Coproduct[WorkflowStep.Outputs](Array("classfile"))
          )
        )
      ))
    workflow.toString.length > 3 shouldBe true
  }

}
