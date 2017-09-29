package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}
import shapeless.Coproduct
import wdl4s.cwl.CommandOutputBinding.Glob
import wdl4s.wdl.types.WdlIntegerType
import eu.timepit.refined._
import wdl4s.wdl.values.{WdlInteger, WdlString}
import wdl4s.wom.expression.PlaceholderIoFunctionSet
import cats.implicits._
import eu.timepit.refined.string.MatchesRegex
import ExpressionEvaluator._

class CommandOutputExpressionSpec extends FlatSpec with Matchers {

  behavior of "CommandOutputExpression"

  it should "evaluateValue" in {
    val tempFile = better.files.File.newTemporaryFile("glob.", ".txt").write("41.1")
    val globExpression = Coproduct[Expression](refineMV[MatchesRegex[ECMAScriptExpressionWitness.T]]("$(inputs.myTempFile)"))
    val outputEvalExpression = Coproduct[Expression](refineMV[MatchesRegex[ECMAScriptExpressionWitness.T]]("$((parseInt(self[0].contents) + 1).toFixed())"))
    val glob = Coproduct[Glob](globExpression)
    val outputEval = Coproduct[StringOrExpression](outputEvalExpression)
    val outputBinding = CommandOutputBinding(Option(glob), Option(true), Option(outputEval))
    val commandOutputExpression = CommandOutputExpression(outputBinding, WdlIntegerType)
    val inputValues = Map("myTempFile" -> WdlString(tempFile.pathAsString))
    val result = commandOutputExpression.evaluateValue(inputValues, PlaceholderIoFunctionSet)
    result should be(WdlInteger(42).valid)
  }

}
