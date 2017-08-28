package wdl4s.cwl

import cats.Apply
import cats.data.Validated.Valid
import cats.syntax.option.catsSyntaxOption
import lenthall.validation.ErrorOr.{ErrorOr, ShortCircuitingFlatMap}
import shapeless.Coproduct
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand}
import wdl4s.cwl.ParametrizedBashParser.Token
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.wom.callable.TaskDefinition

object WomToCwl {

  val parser =
    new ParametrizedBashParser[CommandPart, StringCommandPart,
      ParameterCommandPart](_.isInstanceOf[StringCommandPart], _.literal)

  def optionToErrorOr[A](option: Option[A], message: String): ErrorOr[A] = option.toValidNel(message)

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

  def toArguments(tokenizeResult: parser.TokenizeResult): ErrorOr[Seq[Argument]] = {
    val nonBlanks = tokenizeResult.nonBlankTokens
    if (nonBlanks.size < 2) {
      Valid(Seq.empty)
    } else {
      val indexWhere = nonBlanks.indexWhere(_.string.hasParameters)
      val endIndex = if(indexWhere >= 0) indexWhere else nonBlanks.length
      if (endIndex < 1) {
        Valid(Seq.empty)
      } else {
        val argumentTokens = nonBlanks.slice(1, endIndex)
        val arguments = argumentTokens.map(_.string).flatMap(_.toStringOption).map(Coproduct[Argument](_))
        Valid(arguments)
      }
    }
  }

  def placeholder: ErrorOr[String] = Valid("Yo")

  def toCwl(task: TaskDefinition): ErrorOr[CommandLineTool] = {
    val tokenizeResult = parser.tokenize(task.commandTemplate)
    Apply[ErrorOr].map2(
      toBaseCommand(tokenizeResult),
      toArguments(tokenizeResult)
    )(
      (baseCommand, arguments) =>
        CommandLineTool(
          baseCommand = Option(baseCommand),
          arguments = if(arguments.isEmpty) {
            None
          } else {
            Option(arguments.toArray)
          }
        )
    )
  }

}
