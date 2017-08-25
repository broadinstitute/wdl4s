package wdl4s.cwl

import cats.Apply
import cats.data.NonEmptyList
import cats.data.Validated.Valid
import cats.syntax.option.catsSyntaxOption
import lenthall.validation.ErrorOr.{ErrorOr, ShortCircuitingFlatMap}
import shapeless.Coproduct
import wdl4s.cwl.CommandLineTool.BaseCommand
import wdl4s.cwl.ParametrizedBashParser.Token
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.wom.callable.TaskDefinition

object WomToCwl {

  val parser =
    new ParametrizedBashParser[CommandPart, StringCommandPart,
      ParameterCommandPart](_.isInstanceOf[StringCommandPart], _.literal)

  def optionToErrorOr[A](option: Option[A], message: String): ErrorOr[A] =
    option.toValid(NonEmptyList.of(message))

  def toBaseCommand(tokenizeResult: parser.TokenizeResult): ErrorOr[BaseCommand] = {
    val noNonBlankTokenMessage = "Need non-blank token for base command, but found none"
    val errorOrToken =
      optionToErrorOr(tokenizeResult.nonBlankTokens.headOption, noNonBlankTokenMessage)
    val parameterInBaseNameMessage = "The base name must not have parameters."
    val errorOrBaseString = errorOrToken.flatMap { token: Token[ParameterCommandPart] =>
      optionToErrorOr(token.string.toStringOption, parameterInBaseNameMessage)
    }
    errorOrBaseString.map(baseString => Coproduct[BaseCommand](baseString))
  }

  def placeholder: ErrorOr[String] = Valid("Yo")

  def toCwl(task: TaskDefinition): ErrorOr[CommandLineTool] = {
    val tokens = parser.tokenize(task.commandTemplate)
    Apply[ErrorOr].map2(
      toBaseCommand(tokens),
      placeholder
    )(
      (baseCommand, placeHolder) =>
        CommandLineTool(
          baseCommand = Option(baseCommand)
        )
    )
  }

}
