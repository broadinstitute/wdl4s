package wdl4s.cwl

import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{:+:, CNil, Coproduct}

import io.circe.syntax._
import io.circe._
import io.circe.parser._
import io.circe.shapes._
import io.circe.generic.auto._


/**
  * wdl4s
  * Created by oruebenacker on 03.07.17.
  */
case class AB(a: Int, b: String)

case class ABC(ab: AB, `type`: String :+: Int :+: CNil)

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
    val ab = AB(1, "yo")
    val abc = ABC(ab, Coproduct[String :+: Int :+: CNil](1))
    val abcJson = abc.asJson
    val abcJsonString = abcJson.toString
    println(abcJsonString)
    abcJsonString.length > 3 shouldBe true
//    val toolJson = tool.asJson
//    toolJson.toString.length > 10 shouldBe true
//    //    toolJson.as[CommandLineTool].isRight shouldBe true

    tool.toString.length > 10 shouldBe true
  }

}
