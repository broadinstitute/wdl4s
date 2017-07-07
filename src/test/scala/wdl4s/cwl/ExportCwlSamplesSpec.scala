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
import wdl4s.cwl.CwlVersion.CwlVersion
import io.circe.generic.semiauto.deriveEncoder
import wdl4s.cwl.CwlEncoders._

/**
  * wdl4s
  * Created by oruebenacker on 03.07.17.
  */
sealed trait ABT {
  val cwlVersion: Option[CwlVersion]
}

case class AB(cwlVersion: Option[CwlVersion], b: String)

case class ABC(ab: AB, `type`: String :+: Int :+: CNil)

class ExportCwlSamplesSpec extends FlatSpec with Matchers {

  it should "export mock tool" in {
    val mockTool = MockCommandLineTool(
      inputs = Coproduct[
        MockCommandInputParameter :+:
          Map[MockCommandInputParameter#Id, MockCommandInputParameter#`type`] :+:
          Map[MockCommandInputParameter#Id, MockCommandInputParameter] :+:
          CNil](Map("message" -> MockCommandInputParameter(
        id = None
      ))),
      `class` = CommandLineTool.getClass.getSimpleName,
      cwlVersion = Some(CwlVersion.Version1),
      stdin = None
    )
    val mockToolJson = mockTool.asJson
    val mockToolJsonString = mockToolJson.toString
    println(mockToolJsonString)
    mockToolJsonString.length > 3 shouldBe true
  }

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
    val ab = AB(cwlVersion = Some(CwlVersion.Version1), "yo")
    val versionJson = CwlVersion.Version1.asJson
    val versionJsonString = versionJson.toString
    println(versionJsonString)
    val abJson = ab.asJson
    val abJsonString = abJson.toString
    println(abJsonString)
    abJsonString.length > 3 shouldBe true
//    val toolJson = tool.asJson
//    toolJson.toString.length > 10 shouldBe true
//    toolJson.as[CommandLineTool].isRight shouldBe true
    tool.toString.length > 10 shouldBe true
  }

}
