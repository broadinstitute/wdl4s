package wdl4s.cwl

import io.circe.generic.auto._
import org.scalatest.{FlatSpec, Matchers}
import io.circe.shapes._ // required, but IntelliJ thinks it is unused
import io.circe.syntax._
import io.circe.refined._ // required, but IntelliJ thinks it is unused
import io.circe.literal._ // required, but IntelliJ thinks it is unused
import wdl4s.cwl.CwlEncoders._
import shapeless.Coproduct

class ExportCwlSamplesSpec extends FlatSpec with Matchers {

  it should "export 1st tool" in {
    val tool =
      CommandLineTool(
        inputs = Coproduct[CommandLineTool.Inputs](Map("message" -> CommandInputParameter(
          id = None,
          label = None,
          secondaryFiles = None,
          format = None,
          streamable = None,
          doc = None,
          inputBinding = Some(CommandLineBinding(
            loadContents = None,
            position = Some(1),
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
        cwlVersion = Some(CwlVersion.Version1),
        baseCommand = None,
        arguments = None,
        stdin = None,
        stderr = None,
        stdout = None,
        successCodes = None,
        temporaryFailCodes = None,
        permanentFailCodes = None)
    val toolJson = tool.asJson
    toolJson.toString.length > 100 shouldBe true
  }

}
