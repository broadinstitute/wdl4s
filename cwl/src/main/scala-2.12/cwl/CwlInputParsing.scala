package cwl

import io.circe._
import io.circe.Decoder
import io.circe.shapes._
import io.circe.generic.auto._
import io.circe.refined._
import io.circe.yaml
import io.circe.literal._
import lenthall.validation.Checked._
import wom.executable.Executable.{InputParsingFunction, ParsedInputMap}

object CwlInputParsing {

  implicit val f = implicitly[Decoder[File]]

  // Decodes the input file, and build the ParsedInputMap
  private [cwl] lazy val inputCoercionFunction: InputParsingFunction =
    inputFile => {
      yaml.parser.parse(inputFile).flatMap(_.as[Map[String, MyriadInputValue]]) match {
        case Left(error) => error.getMessage.invalidNelCheck[ParsedInputMap]
        case Right(inputValue) => inputValue.mapValues(_.fold(CwlInputCoercion)).validNelCheck
      }
    }
}
