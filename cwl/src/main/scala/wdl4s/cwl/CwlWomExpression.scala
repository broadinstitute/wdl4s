package wdl4s.cwl

import cats.syntax.validated._
import lenthall.validation.ErrorOr.ErrorOr
import lenthall.validation.Validation._
import wdl4s.cwl.CwlWomExpression.EnhancedParameterContextInputs
import wdl4s.wdl.types.{WdlMapType, WdlNothingType, WdlStringType, WdlType}
import wdl4s.wdl.values.{WdlFile, WdlMap, WdlString, WdlValue}
import wdl4s.wom.expression.{IoFunctionSet, WomExpression}

sealed trait CwlWomExpression extends WomExpression {

  def cwlExpressionType: WdlType

  override def evaluateType(inputTypes: Map[String, WdlType]): ErrorOr[WdlType] = cwlExpressionType.validNel

  override def evaluateFiles(inputTypes: Map[String, WdlValue],
                             ioFunctionSet: IoFunctionSet,
                             coerceTo: WdlType): ErrorOr[Set[WdlFile]] = {
    ???
  }

  // TODO WOM oh geez
  override def inputs: Set[String] = ???
}

case class CommandOutputExpression(outputBinding: CommandOutputBinding,
                                   override val cwlExpressionType: WdlType) extends CwlWomExpression {
  override def evaluateValue(inputValues: Map[String, WdlValue], ioFunctionSet: IoFunctionSet): ErrorOr[WdlValue] = {
    val parameterContext = ParameterContext.Empty.withInputs(inputValues, ioFunctionSet)

    outputBinding match {
      case outputBindingValue =>
        val wdlValue = CommandOutputBindingEvaluator.commandOutputBindingToWdlValue(
          outputBindingValue,
          parameterContext,
          ioFunctionSet
        )
        cwlExpressionType.coerceRawValue(wdlValue).toErrorOr
    }
  }
}

case class StringExpression(expression: String) extends CwlWomExpression {
  override val cwlExpressionType = WdlStringType
  override def evaluateValue(inputValues: Map[String, WdlValue], ioFunctionSet: IoFunctionSet) = {
    val parameterContext = ParameterContext.Empty.withInputs(inputValues, ioFunctionSet)
    // TODO: WOM: Instead of letting exceptions fly, catch and convert to ErrorOr
    ExpressionEvaluator.evalExpression(expression, parameterContext).valid
  }
}

object CwlWomExpression {

  def apply(expression: String): WomExpression = {
    StringExpression(expression)
  }

  implicit class EnhancedParameterContextInputs(val parameterContext: ParameterContext) extends AnyVal {
    def withInputs(inputValues: Map[String, WdlValue], ioFunctionSet: IoFunctionSet): ParameterContext = {
      val wdlValueType = inputValues.values.headOption.map(_.wdlType).getOrElse(WdlNothingType)
      parameterContext.copy(
        inputs = WdlMap(
          WdlMapType(WdlStringType, wdlValueType),
          // TODO: WOM: convert inputValues (including WdlFile?) to inputs using the ioFunctionSet
          inputValues map { case (name, wdlValue) => WdlString(name) -> wdlValue }
        )
      )
    }
  }
}
