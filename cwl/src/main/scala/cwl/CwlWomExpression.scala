package cwl

import cats.data.Validated.Valid
import cats.syntax.validated._
import lenthall.validation.ErrorOr.ErrorOr
import lenthall.validation.Validation._
import wdl.types._
import wdl.values.{WdlArray, WdlFile, WdlGlobFile, WdlMap, WdlString, WdlValue}
import wom.expression.{IoFunctionSet, WomExpression}

sealed trait CwlWomExpression extends WomExpression {

  def cwlExpressionType: WdlType

  override def evaluateType(inputTypes: Map[String, WdlType]): ErrorOr[WdlType] = cwlExpressionType.validNel
}

case class CommandOutputExpression(outputBinding: CommandOutputBinding,
                                   override val cwlExpressionType: WdlType) extends CwlWomExpression {

  override def evaluateValue(inputValues: Map[String, WdlValue], ioFunctionSet: IoFunctionSet): ErrorOr[WdlValue] = {
    val parameterContext = ParameterContext.Empty.withInputs(inputValues, ioFunctionSet)

    val wdlValue = outputBinding.commandOutputBindingToWdlValue(parameterContext, ioFunctionSet)
    cwlExpressionType.coerceRawValue(wdlValue).toErrorOr
  }

  override def inputs: Set[String] = ???

  //TODO: coerceTo is ignored?? we have to return wdlfiles so...
  override def evaluateFiles(inputTypes: Map[String, WdlValue], ioFunctionSet: IoFunctionSet, coerceTo: WdlType): ErrorOr[Set[WdlFile]] ={
    val pc = ParameterContext.Empty.withInputs(inputTypes, ioFunctionSet)
    val wdlValue = outputBinding.commandOutputBindingToWdlValue(pc, ioFunctionSet)
    wdlValue match {
      case WdlArray(WdlMaybeEmptyArrayType(WdlMapType(WdlStringType, WdlStringType)), Seq(WdlMap(WdlMapType(WdlStringType, WdlStringType), map))) =>
        val path = map(WdlString("location")).valueString
        Valid(Set(WdlGlobFile(path)))
      case other =>s":( we saw $other and couldn't convert to a globfile type: ${other.wdlType} coerceTo: $coerceTo".invalidNel[Set[WdlFile]]
    }
  }
}
