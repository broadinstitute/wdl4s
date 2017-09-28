package wdl4s.cwl

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import shapeless.Witness
import wdl4s.wdl.util.JsUtil
import wdl4s.wdl.values.WdlValue

// http://www.commonwl.org/v1.0/CommandLineTool.html#Expressions
object ExpressionEvaluator {
  // A code fragment wrapped in the $(...) syntax must be evaluated as a ECMAScript expression.
  val ECMAScriptExpressionWitness = Witness("""\$\((.*)\)""")
  val ECMAScriptExpressionRegex = ECMAScriptExpressionWitness.value.r
  type ECMAScriptExpression = String Refined MatchesRegex[ECMAScriptExpressionWitness.T]

  // A code fragment wrapped in the ${...} syntax must be evaluated as a ECMAScript function body for an anonymous,
  // zero-argument function.
  val ECMAScriptFunctionWitness = Witness("""\$\{(.*)\}""")
  type ECMAScriptFunction = String Refined MatchesRegex[ECMAScriptFunctionWitness.T]
  val ECMAScriptFunctionRegex = ECMAScriptFunctionWitness.value.r



  def evalExpression(expression: String, parameterContext: ParameterContext): WdlValue = {
    val evalValues = Map(
      "inputs" -> parameterContext.inputs,
      "runtime" -> parameterContext.runtime,
      "self" -> parameterContext.self
    )

    // TODO: WOM: Are we supposed to be trimming the expression just in case before matching?
    expression.trim match {
      case ECMAScriptExpressionRegex(ecmaScriptExpression) => JsUtil.eval(ecmaScriptExpression, evalValues)
      case ECMAScriptFunctionRegex(functionBody) =>
        val functionExpression =
          s"""|(function() {
              |FUNCTION_BODY
              |})();
              |""".stripMargin.replaceAll("FUNCTION_BODY", functionBody)
        JsUtil.eval(functionExpression, evalValues)
      case _ => throw new RuntimeException(s"TODO: WOM: Got unexpected expression '$expression'")
    }
  }
}
