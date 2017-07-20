package wdl4s.cwl

import io.circe._
import io.circe.Encoder
import io.circe.generic.auto._
import org.scalatest.{FlatSpec, Matchers}
import io.circe.shapes._
import io.circe.syntax._
import io.circe.refined._
import io.circe.literal._
import wdl4s.cwl.CwlEncoders._
import shapeless.Coproduct

/**
  * wdl4s
  * Created by oruebenacker on 03.07.17.
  */
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
    val versionJson = CwlVersion.Version1.asJson
    val versionJsonString = versionJson.toString
    println(versionJsonString)
    val toolJson = tool.asJson
    toolJson.toString.length > 10 shouldBe true
    tool.toString.length > 10 shouldBe true
  }

}
