package wdl4s.cwl

import wdl4s.wdl.values.WdlValue
import wdl4s.wom.CommandPart
import wdl4s.wom.expression.IoFunctionSet


case class CwlExpressionCommandPart(expr: Expression) extends CommandPart {
  override def instantiate(inputsMap: Map[String, WdlValue],
                            functions: IoFunctionSet,
                            valueMapper: (WdlValue) => WdlValue): String = {

    //TODO
    //expr.fold(EvaluateExpression).apply(ParameterContext(inputs = ???))
    CwlWomExpression.evaluateExpression(expr, inputsMap, functions).map(_.valueString).getOrElse(???)
  }
}

