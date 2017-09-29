package cwl

import cats.syntax.validated._
import cats.syntax.either._
import io.circe._
import io.circe.shapes._
import io.circe.generic.auto._
import eu.timepit.refined.string._
import io.circe.refined._
import io.circe.yaml
import io.circe.literal._
import lenthall.validation.Checked._
import wom.executable.Executable.{InputParsingFunction, ParsedInputMap}

object CwlInputParsing {

  implicit val x = implicitly[Decoder[File]]

  private [cwl] lazy val inputCoercionFunction: InputParsingFunction =
    inputFile => {
      yaml.parser.parse(inputFile).flatMap(_.as[Map[String, MyriadInputValue]]) match {
        case Left(error) => error.getMessage.invalidNelCheck[ParsedInputMap]
        case Right(inputValue) => inputValue.mapValues(_.fold(CwlInputCoercion)).validNelCheck
      }
    }
}
