package wdl4s.cwl

import cats.Apply
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.Valid
import lenthall.validation.ErrorOr.ErrorOr
import lenthall.validation.ErrorOr.ShortCircuitingFlatMap
import shapeless.Coproduct
import wdl4s.cwl.CommandLineTool.BaseCommand
import wdl4s.cwl.ParametrizedBashParser.Token
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.wom.callable.TaskDefinition

object WomToCwl {

  sealed trait TokenDeprecated {
    def fromCommandPart: CommandPart
  }

  sealed trait StringTokenDeprecated extends TokenDeprecated {
    override def fromCommandPart: StringCommandPart

    def string: String
  }

  case class NonWhitespaceTokenDeprecated(fromCommandPart: StringCommandPart, string: String) extends StringTokenDeprecated

  case class WhitespaceTokenDeprecated(fromCommandPart: StringCommandPart, string: String) extends StringTokenDeprecated

  case class ParameterTokenDeprecated(fromCommandPart: ParameterCommandPart) extends TokenDeprecated

  def toStringTokens(commandPart: StringCommandPart): Seq[StringTokenDeprecated] = {
    val string = commandPart.literal
    var tokens: Seq[StringTokenDeprecated] = Seq.empty
    var previousIsWhitespace = string.charAt(0).isWhitespace
    var tokenString = ""
    for (pos <- 0 until string.length) {
      val nextChar = string.charAt(pos)
      val nowIsWhitespace = string.charAt(pos).isWhitespace
      if (nowIsWhitespace != previousIsWhitespace) {
        val token = if (previousIsWhitespace) {
          WhitespaceTokenDeprecated(commandPart, tokenString)
        } else {
          NonWhitespaceTokenDeprecated(commandPart, tokenString)
        }
        tokens :+= token
        previousIsWhitespace = nowIsWhitespace
        tokenString = ""
      }
      tokenString = tokenString + nextChar
    }
    if (tokenString.nonEmpty) {
      val token = if (tokenString.charAt(0).isWhitespace) {
        WhitespaceTokenDeprecated(commandPart, tokenString)
      } else {
        NonWhitespaceTokenDeprecated(commandPart, tokenString)
      }
      tokens :+= token
    }
    tokens
  }

  val parser =
    new ParametrizedBashParser[CommandPart, StringCommandPart,
      ParameterCommandPart](_.isInstanceOf[ParameterCommandPart], _.literal)

  def toBaseCommand(tokenizeResult: parser.TokenizeResult): ErrorOr[BaseCommand] = {
    val errorOrToken : ErrorOr[ParametrizedBashParser.Token[ParameterCommandPart]] =
      Validated.fromOption(tokenizeResult.nonBlankTokens.headOption,
        NonEmptyList.of("Need non-blank token for base command, but found none"))
    val errorOrBaseString = errorOrToken.flatMap { token: Token[ParameterCommandPart] =>
      Validated.fromOption(token.string.toStringOption, NonEmptyList.of(""))
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
