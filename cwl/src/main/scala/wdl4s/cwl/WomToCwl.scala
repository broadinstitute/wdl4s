package wdl4s.cwl

import cats.Apply
import cats.data.Validated.Valid
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.cwl.CommandLineTool.BaseCommand
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.wom.callable.TaskDefinition

object WomToCwl {

  sealed trait Token {
    def fromCommandPart: CommandPart
  }

  sealed trait StringToken extends Token {
    override def fromCommandPart: StringCommandPart

    def string: String
  }

  case class NonWhitespaceToken(fromCommandPart: StringCommandPart, string: String) extends StringToken

  case class WhitespaceToken(fromCommandPart: StringCommandPart, string: String) extends StringToken

  case class ParameterToken(fromCommandPart: ParameterCommandPart) extends Token

  def toStringTokens(commandPart: StringCommandPart): Seq[StringToken] = {
    val string = commandPart.literal
    var tokens: Seq[StringToken] = Seq.empty
    var previousIsWhitespace = string.charAt(0).isWhitespace
    var tokenString = ""
    for(pos <- 0 until string.length) {
      val nextChar = string.charAt(pos)
      val nowIsWhitespace = string.charAt(pos).isWhitespace
      if(nowIsWhitespace != previousIsWhitespace) {
        val token = if(previousIsWhitespace) {
          WhitespaceToken(commandPart, tokenString)
        } else {
          NonWhitespaceToken(commandPart, tokenString)
        }
        tokens :+= token
        previousIsWhitespace = nowIsWhitespace
        tokenString = ""
      }
      tokenString = tokenString + nextChar
    }
    if(tokenString.nonEmpty) {
      val token = if(tokenString.charAt(0).isWhitespace) {
        WhitespaceToken(commandPart, tokenString)
      } else {
        NonWhitespaceToken(commandPart, tokenString)
      }
      tokens :+= token
    }
    tokens
  }

  def toCwlTokens(task: TaskDefinition): Seq[Token] = ???

  def toBaseCommand(task: TaskDefinition): ErrorOr[BaseCommand] = ???

  def placeholder: ErrorOr[String] = Valid("Yo")

  def toCwl(task: TaskDefinition): ErrorOr[CommandLineTool] =
    Apply[ErrorOr].map2(
      toBaseCommand(task),
      placeholder
    )(
      (baseCommand, placeHolder) =>
        CommandLineTool(
          baseCommand = Option(baseCommand)
        )
    )

}
