package wdl4s.cwl

import io.circe.Encoder
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{:+:, CNil, Coproduct}

/**
  * wdl4s
  * Created by oruebenacker on 03.07.17.
  */
class ExportCwlSamplesSpec extends FlatSpec with Matchers {

  //  implicit val commandLineToolEncoder:Encoder[CommandLineTool] = deriveEncoder[CommandLineTool]

  it should "export 1st tool" in {
    val tool =
      CommandLineTool(
        inputs = Coproduct[
          CommandInputParameter :+:
            Map[CommandInputParameter#Id, CommandInputParameter#`type`] :+:
            Map[CommandInputParameter#Id, CommandInputParameter] :+:
            CNil](Map("message" -> CommandInputParameter(
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
        outputs = Coproduct[Array[CommandOutputParameter] :+:
          Map[CommandOutputParameter#Id, CommandOutputParameter#`type`] :+:
          Map[CommandOutputParameter#Id, CommandOutputParameter] :+:
          CNil](Array.empty[CommandOutputParameter]),
        `class` = CommandLineTool.getClass.getSimpleName,
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
    //    val toolJson = tool.asJson
    //    toolJson.as[CommandLineTool].isRight shouldBe true
    tool.toString.length > 10 shouldBe true
  }

}
