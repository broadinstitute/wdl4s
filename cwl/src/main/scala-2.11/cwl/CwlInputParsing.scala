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
import lenthall.Checked
import lenthall.validation.Checked._
import wom.executable.Executable
import wom.executable.Executable.{InputParsingFunction, ParsedInputMap}

// scala 2.11 doesn't have flatMap on Either, so we need to import cats.syntax.either._
// However it does in scala 2.12 which would make this import unused and fail compilation
// This is the reason why there are 2 versions of this file
object CwlInputParsing {

  implicit val x = implicitly[Decoder[File]]

  private [cwl] val inputCoercionFunction: InputParsingFunction =
    inputFile => {
      yaml.parser.parse(inputFile).flatMap(_.as[Map[String, MyriadInputValue]]) match {
        case Left(error) => error.getMessage.invalidNelCheck[ParsedInputMap]
        case Right(inputValue) => inputValue.map({ case (key, value) => key -> value.fold(CwlInputCoercion) }).validNelCheck
      }
    }
  
  def builWomExecutable(workflow: Workflow, inputFile: Option[String]): Checked[Executable] = {
    for {
      womDefinition <- workflow.womDefinition
      executable <- Executable.withInputs(womDefinition, inputCoercionFunction, inputFile)
    } yield executable
  }
}
