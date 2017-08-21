package wdl4s.cwl

import io.circe._
import io.circe.yaml.{parser => YamlParser}
import io.circe.parser._
import io.circe.shapes._
import io.circe.generic.auto._
import cats.syntax.either._
import eu.timepit.refined.string._
import io.circe.refined._
import io.circe.literal._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._
import cats.syntax.validated._

//import lenthall.validation.ErrorOr.ErrorOr

object CwlCodecs {
  import Implicits._

  //According to automatic derivation, these instances should not be required.  But
  //removing these breaks decodeCwl, so...
  implicit val wfD = implicitly[Decoder[Workflow]]
  implicit val cltD = implicitly[Decoder[CommandLineTool]]

  def decodeCwl(in: String) = decode[Cwl](in)

  /**
    * Parse a possibly top-level CWL file and return representations of this file and any referenced files.  This is
    * very simplistic logic that assumes only one level of depth max.
    * @return a `Map[String, Cwl]` of filenames to `Cwl`s.
    */
   /*
  def decodeCwl(yaml: String): EitherT[IO, NonEmptyList[String], Cwl] = {
    decodeSingleFileCwl(yaml) match {
      case Right(clt: CommandLineTool) => (clt, Map.empty[String, Cwl]).validNel
      case Right(wf: Workflow) =>

        wf.steps.traverse{
          wfStep =>
            wfStep.filename.

        }

        val fileNameToFiles: EitherA[List[(String, CwlFile)]] = fileNames.traverse[ErrorOr, (String, CwlFile)] {
          fileName =>
            val yaml = scala.io.Source.fromFile(fileName).getLines.mkString("\n")
            decodeCwl(yaml).map(fileName -> _)
        }

        fileNameToFiles.map(_.toMap).map(wf -> _) match {
          case Left(e) => e.getMessage.invalidNel
          case Right(f) => f.validNel
        }
      case Left(e) => e.getMessage.invalidNel
    }
  }

  private def decodeSingleFileCwl: String => EitherA[CwlFile] = {
    import wdl4s.cwl.Implicits._

    YamlParser.
      parse(_).
      map(_.noSpaces).
      flatMap { json =>
        decode[CommandLineTool](json) orElse decode[Workflow](json)
      }
  }

  def encodeCwlCommandLineTool(commandLineTool: CommandLineTool): Json = {
    import io.circe.syntax._
    import wdl4s.cwl.Implicits.enumerationEncoder
    commandLineTool.asJson
  }

  def encodeCwlWorkflow(workflow: Workflow): Json = {
    import io.circe.syntax._
    import wdl4s.cwl.Implicits.enumerationEncoder
    workflow.asJson
  }

  def encodeCwl(cwl: CwlFile): Json = {
    import io.circe.syntax._
    import wdl4s.cwl.Implicits.enumerationEncoder
    cwl match {
      case commandLineTool: CommandLineTool => commandLineTool.asJson
      case workflow: Workflow => workflow.asJson
    }
  }

  val jsonPrettyPrinter = io.circe.Printer.spaces2.copy(dropNullKeys = true, preserveOrder = true)
  val yamlPrettyPrinter = io.circe.yaml.Printer.spaces2.copy(dropNullKeys = true, preserveOrder = true)

  def cwlToJson(cwl: CwlFile): String = jsonPrettyPrinter.pretty(encodeCwl(cwl))

  def cwlToYaml(cwl: CwlFile): String = yamlPrettyPrinter.pretty(encodeCwl(cwl))
  */

}
