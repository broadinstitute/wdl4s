package wdl4s.cwl

import io.circe._
import io.circe.generic.auto._
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

object CwlCodecs {

  type EitherA[A] = Either[Error, A]

  /**
    * Parse a possibly top-level CWL file and return representations of this file and any referenced files.  This is
    * a very temporary structure which assumes only one level of depth max.
    * @return a `Map[String, CwlFile]` of filenames to `CwlFile`s.
    */
  def decodeCwl: Yaml => EitherA[(CwlFile, Map[String, CwlFile])] = {
    decodeSingleFileCwl(_).flatMap {
      case clt: CommandLineTool => Right((clt, Map.empty))
      case wf: Workflow =>

        val fileNames: List[String] = wf.steps.toList.flatMap(_.run.select[String].toList)

        val r: EitherA[List[(String, CwlFile)]] = fileNames.traverse[EitherA, (String, CwlFile)] {
          fileName =>
            val yaml = scala.io.Source.fromFile(fileName).getLines.mkString("\n")
            // TODO: This should recurse and decodeSingleFileCwl should go away, though then we would have to return some
            // sort of tree structure rather than this flat map.
            decodeSingleFileCwl(yaml).map(fileName -> _)
        }

        r.map(_.toMap).map(wf -> _)
    }
  }

  private def decodeSingleFileCwl: Yaml => EitherA[CwlFile] = {
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

  def cwlToYaml(cwl: CwlFile): Yaml = yamlPrettyPrinter.pretty(encodeCwl(cwl))

}
