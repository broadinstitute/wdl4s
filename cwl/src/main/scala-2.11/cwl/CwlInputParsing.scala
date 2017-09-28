package cwl.cwl

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
  implicit val fileD = implicitly[Decoder[File]]
  implicit val stringD = implicitly[Decoder[String]]
  implicit val intD = implicitly[Decoder[Int]]
  implicit val longD = implicitly[Decoder[Long]]
  implicit val floatD = implicitly[Decoder[Float]]
  implicit val doubleD = implicitly[Decoder[Double]]
  implicit val booleanD = implicitly[Decoder[Boolean]]
  implicit val inputArray = implicitly[Decoder[Array[MyriadInputValuePrimitives]]]

  // Decodes the input file, and build the ParsedInputMap
  private [cwl] lazy val inputCoercionFunction: InputParsingFunction = inputFile => {
    yaml.parser.parse(inputFile).flatMap(_.as[Map[String, MyriadInputValue]]) match {
      case Left(error) => error.getMessage.invalidNelCheck[ParsedInputMap]
      case Right(inputValue) => inputValue.mapValues(_.fold(CwlInputCoercion)).validNelCheck
    }
  }
}
