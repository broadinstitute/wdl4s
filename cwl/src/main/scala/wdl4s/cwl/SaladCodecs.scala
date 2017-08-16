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
import cats.syntax.validated._

import lenthall.validation.ErrorOr.ErrorOr

object SaladDecoder {

  type EitherA[A] = Either[Error, A]

  def decodeSingleFileJson: String => EitherA[CwlFile] =
    json => {
      import wdl4s.cwl.Implicits._

      decode[CommandLineTool](json) orElse decode[Workflow](json)
    }
}
