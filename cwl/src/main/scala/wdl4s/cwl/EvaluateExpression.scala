package wdl4s.cwl

import shapeless.Poly1
import wdl4s.cwl.ExpressionEvaluator.{ECMAScriptExpression, ECMAScriptFunction}

object EvaluateExpression extends Poly1 {
  implicit def script = at[ECMAScriptExpression] { e =>
    (parameterContext: ParameterContext) =>
      ExpressionEvaluator.evalExpression(e, parameterContext)
  }

  implicit def function = at[ECMAScriptFunction] { f =>
    (parameterContext: ParameterContext) =>
      ExpressionEvaluator.evalFunction(f, parameterContext)
  }
}
