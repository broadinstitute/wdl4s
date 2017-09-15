package wdl4s.cwl

import cats.Apply
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option.catsSyntaxOption
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.cwl.ParametrizedBashParser.Token


object CwlCmdTokenGrouper {

  sealed trait Group[+P] {
    def pos: Int
  }

  object Group {

    case class BaseCommand(string: String) extends Group[Nothing] {
      override def pos: Int = -1
    }

    case class Argument[P](pos: Int, string: String) extends Group[Nothing]

    case class InputBinding[P](pos: Int, prefixOption: Option[String], separated: Boolean,
                               parameter: P) extends Group[P]

  }

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
      val errorOrBaseCommandGroup = errorOrBaseCommandString.map(Group.BaseCommand)
      errorOrGroups = plusGroup(errorOrGroups, errorOrBaseCommandGroup)
      var index = 0
      var nextTokenOption: Option[Token[P]] = None
      var nextNextTokenOption: Option[Token[P]] = None

      def advanceTokenIterator(): Unit = {
        index += 1
        nextTokenOption = nextNextTokenOption
        nextNextTokenOption = if (tokensIter.hasNext) Option(tokensIter.next()) else None
      }

      advanceTokenIterator()
      advanceTokenIterator()
      index = 0
      while (nextTokenOption.nonEmpty) {
        val errorOrGroup = (nextTokenOption, nextNextTokenOption) match {
          case (Some(token), _) if token.string.hasParameters =>
            if (token.string.nParameters > 1) {
              Invalid(NonEmptyList.of("Cannot handle tokens with multiple parameters"))
            } else {
              val parameter = token.string.parameters.head
              if (token.string.isBareParameter) {
                Valid(Group.InputBinding(index, None, separated = false, parameter))
              } else if (token.string.isPrefixedParameter) {
                val prefix = token.string.prefix
                Valid(Group.InputBinding(index, Option(prefix), separated = false, parameter))
              } else {
                Invalid(NonEmptyList.of("Can only handle bare and prefixed, not postfixed parameters."))
              }
            }
          case (Some(prefixToken), Some(parameterToken))
            if !prefixToken.string.hasParameters && parameterToken.string.isBareParameter =>
            val prefix = prefixToken.string.prefix
            val parameter = parameterToken.string.parameters.head
            advanceTokenIterator()
            Valid(Group.InputBinding(index, Option(prefix), separated = true, parameter))
          case (Some(token), _) if !token.string.hasParameters =>
            val argumentString = token.string.prefix
            Valid(Group.Argument(index, argumentString))
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
