package wdl4s.cwl

import cats.data.Validated.Valid
import cats.syntax.option.catsSyntaxOption
import lenthall.validation.ErrorOr.{ErrorOr, ShortCircuitingFlatMap}
import shapeless.Coproduct
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand, StringOrExpression}
import wdl4s.cwl.CwlCmdTokenGrouper.Group
import wdl4s.cwl.ParametrizedBashParser.Token
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.wom.callable.TaskDefinition

object WomToCwl {

  val parser =
    new ParametrizedBashParser[CommandPart, StringCommandPart,
      ParameterCommandPart](_.isInstanceOf[StringCommandPart], _.literal)

  val grouper = CwlCmdTokenGrouper

  def toBaseCommand(tokenizeResult: parser.TokenizeResult): ErrorOr[BaseCommand] = {
    val noNonBlankTokenMessage = "Need non-blank token for base command, but found none"
    val errorOrToken =
      tokenizeResult.nonBlankTokens.headOption.toValidNel(noNonBlankTokenMessage)
    val parameterInBaseNameMessage = "The base name must not have parameters."
    val errorOrBaseString = errorOrToken.flatMap { token: Token[ParameterCommandPart] =>
      token.string.toStringOption.toValidNel(parameterInBaseNameMessage)
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

  def toCwl(task: TaskDefinition): ErrorOr[CommandLineTool] = {
    val tokenizeResult = parser.tokenize(task.commandTemplate)
    val groups = grouper.groupTokens(tokenizeResult.nonBlankTokens)
    val errorOrCommandLineTool = groups.map { groups =>
      val baseCommand = Option(Coproduct[BaseCommand](groups.head.asInstanceOf[Group.BaseCommand].string))
      val arguments = Option(groups.collect {
        case Group.Argument(pos, string) =>
          val position = Option(pos)
          val valueFrom = Option(Coproduct[StringOrExpression](string))
          Coproduct[Argument](CommandLineBinding(position = position, valueFrom = valueFrom))
      }.toArray)
      val inputs = groups.collect {
        case Group.InputBinding(pos, prefixOption, separate, parameter) =>
          val id = s"input$pos"
          val valueFrom = Option(Coproduct[StringOrExpression](parameter.expression.toWdlString))
          val inputBinding = Option(
            CommandLineBinding(
              position = Option(pos),
              prefix = prefixOption,
              separate = if(separate) Option("true") else Option("false"),
              valueFrom = valueFrom
            )
          )
          CommandInputParameter(
            id = id,
            inputBinding = inputBinding
          )
      }.toArray
      CommandLineTool(
        inputs = inputs,
        baseCommand = baseCommand,
        arguments = arguments
      )
    }
    errorOrCommandLineTool
  }

}
