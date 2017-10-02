package cwl

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import shapeless.Witness
import wdl.util.JsUtil
import wdl.values.WdlValue

// http://www.commonwl.org/v1.0/CommandLineTool.html#Expressions
object ExpressionEvaluator {
  // A code fragment wrapped in the $(...) syntax must be evaluated as a ECMAScript expression.
  val ECMAScriptExpressionWitness = Witness("""\$\((.*)\)""")
  type MatchesECMAScript = MatchesRegex[ECMAScriptExpressionWitness.T]
  type ECMAScriptExpression = String Refined MatchesRegex[ECMAScriptExpressionWitness.T]
  val ECMAScriptExpressionRegex = ECMAScriptExpressionWitness.value.r

  // A code fragment wrapped in the ${...} syntax must be evaluated as a ECMAScript function body for an anonymous,
  // zero-argument function.
  val ECMAScriptFunctionWitness = Witness("""\$\{(.*)\}""")
  type ECMAScriptFunction = String Refined MatchesRegex[ECMAScriptFunctionWitness.T]
  type MatchesECMAFunction = MatchesRegex[ECMAScriptFunctionWitness.T]


  def evalExpression(expression: ECMAScriptExpression, parameterContext: ParameterContext): WdlValue =
    expression.value match {
      case ECMAScriptExpressionRegex(script) => JsUtil.eval(script, paramValues(parameterContext))
    }

  def evalFunction(function: ECMAScriptFunction, parameterContext: ParameterContext): WdlValue = {
    val functionExpression =
      s"""|(function() {
          |FUNCTION_BODY
          |})();
          |""".stripMargin.replaceAll("FUNCTION_BODY", function.value)

    JsUtil.eval(functionExpression, paramValues(parameterContext))
  }

  def paramValues(parameterContext: ParameterContext) =
    Map(
      "inputs" -> parameterContext.inputs,
      "runtime" -> parameterContext.runtime,
      "self" -> parameterContext.self
    )

}
