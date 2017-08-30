package wdl4s.cwl

import cats.Apply
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option.catsSyntaxOption
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.cwl.ParametrizedBashParser.Token


object CwlCmdTokenGrouper {

  sealed trait Group[+P]

  case class BaseCommand(string: String) extends Group[Nothing]

  case class Argument[P](string: String) extends Group[Nothing]

  case class InputBinding[P](prefixOption: Option[String], separated: Boolean,
                             parameter: P) extends Group[P]

  def groupTokens[P](tokens: Seq[Token[P]]): ErrorOr[Seq[Group[P]]] = {
    val tokensIter = tokens.iterator
    if (tokensIter.hasNext) {
      var errorOrGroups: ErrorOr[Seq[Group[P]]] = Valid(Seq.empty)
      val firstToken = tokensIter.next

      def plusGroup(errorOrGroups: ErrorOr[Seq[Group[P]]], errorOrGroup: ErrorOr[Group[P]]):
      ErrorOr[Seq[Group[P]]] = {
        Apply[ErrorOr].map2(errorOrGroups, errorOrGroup)(_ :+ _)
      }

      val errorOrBaseCommandString =
        firstToken.string.toStringOption.toValidNel("First token must have no parameters")
      val errorOrBaseCommandGroup = errorOrBaseCommandString.map(BaseCommand)
      errorOrGroups = plusGroup(errorOrGroups, errorOrBaseCommandGroup)
      var nextTokenOption: Option[Token[P]] = None
      var nextNextTokenOption: Option[Token[P]] = None

      def advanceTokenIterator(): Unit = {
        nextTokenOption = nextNextTokenOption
        nextNextTokenOption = if (tokensIter.hasNext) Option(tokensIter.next()) else None
      }

      advanceTokenIterator()
      advanceTokenIterator()
      while (nextTokenOption.nonEmpty) {
        val errorOrGroup = (nextTokenOption, nextNextTokenOption) match {
          case (Some(token), _) if token.string.hasParameters =>
            if (token.string.nParameters > 1) {
              Invalid(NonEmptyList.of("Cannot handle tokens with multiple parameters"))
            } else {
              val parameter = token.string.parameters.head
              if (token.string.isBareParameter) {
                Valid(InputBinding(None, separated = false, parameter))
              } else if (token.string.isPrefixedParameter) {
                val prefix = token.string.prefix
                Valid(InputBinding(Option(prefix), separated = false, parameter))
              } else {
                Invalid(NonEmptyList.of("Can only handle bare and prefixed, not postfixed parameters."))
              }
            }
          case (Some(prefixToken), Some(parameterToken))
            if !prefixToken.string.hasParameters && parameterToken.string.isBareParameter =>
            val prefix = prefixToken.string.prefix
            val parameter = parameterToken.string.parameters.head
            advanceTokenIterator()
            Valid(InputBinding(Option(prefix), separated = true, parameter))
          case (Some(token), _) if !token.string.hasParameters =>
            val argumentString = token.string.prefix
            Valid(Argument(argumentString))
          case (Some(token), _) =>
            Invalid(NonEmptyList.of(s"Cannot handle token $token"))
        }
        errorOrGroups = plusGroup(errorOrGroups, errorOrGroup)
        advanceTokenIterator()
      }
      errorOrGroups
    } else {
      Invalid(NonEmptyList.of("No tokens."))
    }
  }

}
