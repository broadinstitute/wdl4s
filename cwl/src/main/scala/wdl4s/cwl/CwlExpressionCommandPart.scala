package wdl4s.cwl

import wdl4s.wdl.types.{WdlAnyType, WdlNonEmptyArrayType, WdlPairType, WdlStringType}
import wdl4s.wdl.values.{WdlArray, WdlPair, WdlString, WdlValue}
import wdl4s.wom.CommandPart
import wdl4s.wom.expression.IoFunctionSet

import CwlWomExpression._


case class CwlExpressionCommandPart(expr: Expression) extends CommandPart {
  override def instantiate(inputsMap: Map[String, WdlValue],
                            functions: IoFunctionSet,
                            valueMapper: (WdlValue) => WdlValue): String = {

    val pc = ParameterContext.Empty.withInputs(inputsMap, functions)

    val wdlValue: WdlValue = expr.fold(EvaluateExpression).apply(pc)

    wdlValue.valueString
  }

  def paramValues(inputsMap: Map[String, WdlValue]) : ParameterContext = {
    val wdlPairs = inputsMap.map { case (key, value) => WdlPair(WdlString(key), value)}

    val wdlArray: WdlArray = WdlArray(WdlNonEmptyArrayType(WdlPairType(WdlStringType, WdlAnyType)), wdlPairs.toSeq)

    ParameterContext(inputs =wdlArray)
  }
}

