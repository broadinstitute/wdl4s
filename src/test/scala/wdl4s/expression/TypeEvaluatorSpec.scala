package wdl4s.expression

import wdl4s.types._
import wdl4s.values._
import wdl4s.{SampleWdl, WdlNamespaceWithWorkflow, WdlExpression}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class TypeEvaluatorSpec extends FlatSpec with Matchers {
  val expr: String => WdlExpression = WdlExpression.fromString
  val namespace = WdlNamespaceWithWorkflow.load(SampleWdl.ThreeStep.wdlSource())

  def noLookup(String: String): WdlType = fail("No identifiers should be looked up in this test")

  def identifierLookup(String: String): WdlType = {
    String match {
      case "cgrep" => WdlCallOutputsObjectType(namespace.workflow.calls.find(_.unqualifiedName == "cgrep").get)
      case "ps" => WdlCallOutputsObjectType(namespace.workflow.calls.find(_.unqualifiedName == "ps").get)
    }
  }

  def identifierEval(exprStr: String): WdlPrimitiveType = expr(exprStr).evaluateType(identifierLookup, new WdlStandardLibraryFunctionsType).asInstanceOf[Try[WdlPrimitiveType]].get
  def identifierEvalError(exprStr: String): Unit = {
    expr(exprStr).evaluateType(identifierLookup, new WdlStandardLibraryFunctionsType).asInstanceOf[Try[WdlPrimitive]] match {
      case Failure(ex) => // Expected
      case Success(badValue) => fail(s"Operation was supposed to fail, instead I got value: $badValue")
    }
  }

  private def operate(lhs: WdlType, op: String, rhs: WdlType): Try[WdlType] = op match {
    case "+" => lhs.add(rhs)
    case "-" => lhs.subtract(rhs)
    case "*" => lhs.multiply(rhs)
    case "/" => lhs.divide(rhs)
    case "%" => lhs.mod(rhs)
    case "==" => lhs.equals(rhs)
    case "!=" => lhs.notEquals(rhs)
    case "<" => lhs.lessThan(rhs)
    case "<=" => lhs.lessThanOrEqual(rhs)
    case ">" => lhs.greaterThan(rhs)
    case ">=" => lhs.greaterThanOrEqual(rhs)
    case "||" => lhs.or(rhs)
    case "&&" => lhs.and(rhs)
    case _ => fail(s"unexpected operator: $op")
  }

  val validOperations = Table(
    ("lhs", "op", "rhs", "result"),
    (WdlIntegerType, "+", WdlIntegerType, WdlIntegerType),
    (WdlIntegerType, "+", WdlFloatType, WdlFloatType),
    (WdlIntegerType, "+", WdlStringType, WdlStringType),
    (WdlIntegerType, "-", WdlIntegerType, WdlIntegerType),
    (WdlIntegerType, "-", WdlFloatType, WdlFloatType),
    (WdlIntegerType, "*", WdlIntegerType, WdlIntegerType),
    (WdlIntegerType, "*", WdlFloatType, WdlFloatType),
    (WdlIntegerType, "/", WdlIntegerType, WdlIntegerType),
    (WdlIntegerType, "/", WdlFloatType, WdlFloatType),
    (WdlIntegerType, "%", WdlIntegerType, WdlIntegerType),
    (WdlIntegerType, "%", WdlFloatType, WdlFloatType),
    (WdlIntegerType, "==", WdlIntegerType, WdlBooleanType),
    (WdlIntegerType, "==", WdlFloatType, WdlBooleanType),
    (WdlIntegerType, "!=", WdlIntegerType, WdlBooleanType),
    (WdlIntegerType, "!=", WdlFloatType, WdlBooleanType),
    (WdlIntegerType, "<", WdlIntegerType, WdlBooleanType),
    (WdlIntegerType, "<", WdlFloatType, WdlBooleanType),
    (WdlIntegerType, "<=", WdlIntegerType, WdlBooleanType),
    (WdlIntegerType, "<=", WdlFloatType, WdlBooleanType),
    (WdlIntegerType, ">", WdlIntegerType, WdlBooleanType),
    (WdlIntegerType, ">", WdlFloatType, WdlBooleanType),
    (WdlIntegerType, ">=", WdlIntegerType, WdlBooleanType),
    (WdlIntegerType, ">=", WdlFloatType, WdlBooleanType),
    (WdlFloatType, "+", WdlIntegerType, WdlFloatType),
    (WdlFloatType, "+", WdlFloatType, WdlFloatType),
    (WdlFloatType, "+", WdlStringType, WdlStringType),
    (WdlFloatType, "-", WdlIntegerType, WdlFloatType),
    (WdlFloatType, "-", WdlFloatType, WdlFloatType),
    (WdlFloatType, "*", WdlIntegerType, WdlFloatType),
    (WdlFloatType, "*", WdlFloatType, WdlFloatType),
    (WdlFloatType, "/", WdlIntegerType, WdlFloatType),
    (WdlFloatType, "/", WdlFloatType, WdlFloatType),
    (WdlFloatType, "%", WdlIntegerType, WdlFloatType),
    (WdlFloatType, "%", WdlFloatType, WdlFloatType),
    (WdlFloatType, "==", WdlIntegerType, WdlBooleanType),
    (WdlFloatType, "==", WdlFloatType, WdlBooleanType),
    (WdlFloatType, "!=", WdlIntegerType, WdlBooleanType),
    (WdlFloatType, "!=", WdlFloatType, WdlBooleanType),
    (WdlFloatType, "<", WdlIntegerType, WdlBooleanType),
    (WdlFloatType, "<", WdlFloatType, WdlBooleanType),
    (WdlFloatType, "<=", WdlIntegerType, WdlBooleanType),
    (WdlFloatType, "<=", WdlFloatType, WdlBooleanType),
    (WdlFloatType, ">", WdlIntegerType, WdlBooleanType),
    (WdlFloatType, ">", WdlFloatType, WdlBooleanType),
    (WdlFloatType, ">=", WdlIntegerType, WdlBooleanType),
    (WdlFloatType, ">=", WdlFloatType, WdlBooleanType),
    (WdlStringType, "+", WdlIntegerType, WdlStringType),
    (WdlStringType, "+", WdlFloatType, WdlStringType),
    (WdlStringType, "+", WdlStringType, WdlStringType),
    (WdlStringType, "+", WdlFileType, WdlStringType),
    (WdlStringType, "==", WdlStringType, WdlBooleanType),
    (WdlStringType, "!=", WdlStringType, WdlBooleanType),
    (WdlStringType, "<", WdlStringType, WdlBooleanType),
    (WdlStringType, "<=", WdlStringType, WdlBooleanType),
    (WdlStringType, ">", WdlStringType, WdlBooleanType),
    (WdlStringType, ">=", WdlStringType, WdlBooleanType),
    (WdlFileType, "+", WdlStringType, WdlFileType),
    (WdlFileType, "==", WdlStringType, WdlBooleanType),
    (WdlFileType, "==", WdlFileType, WdlBooleanType),
    (WdlFileType, "!=", WdlStringType, WdlBooleanType),
    (WdlFileType, "!=", WdlFileType, WdlBooleanType),
    (WdlFileType, "<=", WdlStringType, WdlBooleanType),
    (WdlFileType, "<=", WdlFileType, WdlBooleanType),
    (WdlFileType, ">=", WdlStringType, WdlBooleanType),
    (WdlFileType, ">=", WdlFileType, WdlBooleanType),
    (WdlBooleanType, "==", WdlBooleanType, WdlBooleanType),
    (WdlBooleanType, "!=", WdlBooleanType, WdlBooleanType),
    (WdlBooleanType, "<", WdlBooleanType, WdlBooleanType),
    (WdlBooleanType, "<=", WdlBooleanType, WdlBooleanType),
    (WdlBooleanType, ">", WdlBooleanType, WdlBooleanType),
    (WdlBooleanType, ">=", WdlBooleanType, WdlBooleanType),
    (WdlBooleanType, "||", WdlBooleanType, WdlBooleanType),
    (WdlBooleanType, "&&", WdlBooleanType, WdlBooleanType)
  )

  val invalidOperations = Table(
    ("lhs", "op", "rhs"),
    (WdlIntegerType, "+", WdlFileType),
    (WdlIntegerType, "+", WdlBooleanType),
    (WdlIntegerType, "-", WdlStringType),
    (WdlIntegerType, "-", WdlFileType),
    (WdlIntegerType, "-", WdlBooleanType),
    (WdlIntegerType, "*", WdlStringType),
    (WdlIntegerType, "*", WdlFileType),
    (WdlIntegerType, "*", WdlBooleanType),
    (WdlIntegerType, "/", WdlStringType),
    (WdlIntegerType, "/", WdlFileType),
    (WdlIntegerType, "/", WdlBooleanType),
    (WdlIntegerType, "%", WdlStringType),
    (WdlIntegerType, "%", WdlFileType),
    (WdlIntegerType, "%", WdlBooleanType),
    (WdlIntegerType, "==", WdlStringType),
    (WdlIntegerType, "==", WdlFileType),
    (WdlIntegerType, "==", WdlBooleanType),
    (WdlIntegerType, "!=", WdlStringType),
    (WdlIntegerType, "!=", WdlFileType),
    (WdlIntegerType, "!=", WdlBooleanType),
    (WdlIntegerType, "<", WdlStringType),
    (WdlIntegerType, "<", WdlFileType),
    (WdlIntegerType, "<", WdlBooleanType),
    (WdlIntegerType, "<=", WdlStringType),
    (WdlIntegerType, "<=", WdlFileType),
    (WdlIntegerType, "<=", WdlBooleanType),
    (WdlIntegerType, ">", WdlStringType),
    (WdlIntegerType, ">", WdlFileType),
    (WdlIntegerType, ">", WdlBooleanType),
    (WdlIntegerType, ">=", WdlStringType),
    (WdlIntegerType, ">=", WdlFileType),
    (WdlIntegerType, ">=", WdlBooleanType),
    (WdlIntegerType, "||", WdlIntegerType),
    (WdlIntegerType, "||", WdlFloatType),
    (WdlIntegerType, "||", WdlStringType),
    (WdlIntegerType, "||", WdlFileType),
    (WdlIntegerType, "||", WdlBooleanType),
    (WdlIntegerType, "&&", WdlIntegerType),
    (WdlIntegerType, "&&", WdlFloatType),
    (WdlIntegerType, "&&", WdlStringType),
    (WdlIntegerType, "&&", WdlFileType),
    (WdlIntegerType, "&&", WdlBooleanType),
    (WdlFloatType, "+", WdlFileType),
    (WdlFloatType, "+", WdlBooleanType),
    (WdlFloatType, "-", WdlStringType),
    (WdlFloatType, "-", WdlFileType),
    (WdlFloatType, "-", WdlBooleanType),
    (WdlFloatType, "*", WdlStringType),
    (WdlFloatType, "*", WdlFileType),
    (WdlFloatType, "*", WdlBooleanType),
    (WdlFloatType, "/", WdlStringType),
    (WdlFloatType, "/", WdlFileType),
    (WdlFloatType, "/", WdlBooleanType),
    (WdlFloatType, "%", WdlStringType),
    (WdlFloatType, "%", WdlFileType),
    (WdlFloatType, "%", WdlBooleanType),
    (WdlFloatType, "==", WdlStringType),
    (WdlFloatType, "==", WdlFileType),
    (WdlFloatType, "==", WdlBooleanType),
    (WdlFloatType, "!=", WdlStringType),
    (WdlFloatType, "!=", WdlFileType),
    (WdlFloatType, "!=", WdlBooleanType),
    (WdlFloatType, "<", WdlStringType),
    (WdlFloatType, "<", WdlFileType),
    (WdlFloatType, "<", WdlBooleanType),
    (WdlFloatType, "<=", WdlStringType),
    (WdlFloatType, "<=", WdlFileType),
    (WdlFloatType, "<=", WdlBooleanType),
    (WdlFloatType, ">", WdlStringType),
    (WdlFloatType, ">", WdlFileType),
    (WdlFloatType, ">", WdlBooleanType),
    (WdlFloatType, ">=", WdlStringType),
    (WdlFloatType, ">=", WdlFileType),
    (WdlFloatType, ">=", WdlBooleanType),
    (WdlFloatType, "||", WdlIntegerType),
    (WdlFloatType, "||", WdlFloatType),
    (WdlFloatType, "||", WdlStringType),
    (WdlFloatType, "||", WdlFileType),
    (WdlFloatType, "||", WdlBooleanType),
    (WdlFloatType, "&&", WdlIntegerType),
    (WdlFloatType, "&&", WdlFloatType),
    (WdlFloatType, "&&", WdlStringType),
    (WdlFloatType, "&&", WdlFileType),
    (WdlFloatType, "&&", WdlBooleanType),
    (WdlStringType, "+", WdlBooleanType),
    (WdlStringType, "-", WdlIntegerType),
    (WdlStringType, "-", WdlFloatType),
    (WdlStringType, "-", WdlStringType),
    (WdlStringType, "-", WdlFileType),
    (WdlStringType, "-", WdlBooleanType),
    (WdlStringType, "*", WdlIntegerType),
    (WdlStringType, "*", WdlFloatType),
    (WdlStringType, "*", WdlStringType),
    (WdlStringType, "*", WdlFileType),
    (WdlStringType, "*", WdlBooleanType),
    (WdlStringType, "/", WdlIntegerType),
    (WdlStringType, "/", WdlFloatType),
    (WdlStringType, "/", WdlStringType),
    (WdlStringType, "/", WdlFileType),
    (WdlStringType, "/", WdlBooleanType),
    (WdlStringType, "%", WdlIntegerType),
    (WdlStringType, "%", WdlFloatType),
    (WdlStringType, "%", WdlStringType),
    (WdlStringType, "%", WdlFileType),
    (WdlStringType, "%", WdlBooleanType),
    (WdlStringType, "==", WdlIntegerType),
    (WdlStringType, "==", WdlFloatType),
    (WdlStringType, "==", WdlFileType),
    (WdlStringType, "==", WdlBooleanType),
    (WdlStringType, "!=", WdlIntegerType),
    (WdlStringType, "!=", WdlFloatType),
    (WdlStringType, "!=", WdlFileType),
    (WdlStringType, "!=", WdlBooleanType),
    (WdlStringType, "<", WdlIntegerType),
    (WdlStringType, "<", WdlFloatType),
    (WdlStringType, "<", WdlFileType),
    (WdlStringType, "<", WdlBooleanType),
    (WdlStringType, "<=", WdlIntegerType),
    (WdlStringType, "<=", WdlFloatType),
    (WdlStringType, "<=", WdlFileType),
    (WdlStringType, "<=", WdlBooleanType),
    (WdlStringType, ">", WdlIntegerType),
    (WdlStringType, ">", WdlFloatType),
    (WdlStringType, ">", WdlFileType),
    (WdlStringType, ">", WdlBooleanType),
    (WdlStringType, ">=", WdlIntegerType),
    (WdlStringType, ">=", WdlFloatType),
    (WdlStringType, ">=", WdlFileType),
    (WdlStringType, ">=", WdlBooleanType),
    (WdlStringType, "||", WdlIntegerType),
    (WdlStringType, "||", WdlFloatType),
    (WdlStringType, "||", WdlStringType),
    (WdlStringType, "||", WdlFileType),
    (WdlStringType, "||", WdlBooleanType),
    (WdlStringType, "&&", WdlIntegerType),
    (WdlStringType, "&&", WdlFloatType),
    (WdlStringType, "&&", WdlStringType),
    (WdlStringType, "&&", WdlFileType),
    (WdlStringType, "&&", WdlBooleanType),
    (WdlFileType, "+", WdlFileType),
    (WdlFileType, "+", WdlIntegerType),
    (WdlFileType, "+", WdlFloatType),
    (WdlFileType, "+", WdlBooleanType),
    (WdlFileType, "-", WdlIntegerType),
    (WdlFileType, "-", WdlFloatType),
    (WdlFileType, "-", WdlStringType),
    (WdlFileType, "-", WdlFileType),
    (WdlFileType, "-", WdlBooleanType),
    (WdlFileType, "*", WdlIntegerType),
    (WdlFileType, "*", WdlFloatType),
    (WdlFileType, "*", WdlStringType),
    (WdlFileType, "*", WdlFileType),
    (WdlFileType, "*", WdlBooleanType),
    (WdlFileType, "/", WdlIntegerType),
    (WdlFileType, "/", WdlFloatType),
    (WdlFileType, "/", WdlStringType),
    (WdlFileType, "/", WdlFileType),
    (WdlFileType, "/", WdlBooleanType),
    (WdlFileType, "%", WdlIntegerType),
    (WdlFileType, "%", WdlFloatType),
    (WdlFileType, "%", WdlStringType),
    (WdlFileType, "%", WdlFileType),
    (WdlFileType, "%", WdlBooleanType),
    (WdlFileType, "==", WdlIntegerType),
    (WdlFileType, "==", WdlFloatType),
    (WdlFileType, "==", WdlBooleanType),
    (WdlFileType, "!=", WdlIntegerType),
    (WdlFileType, "!=", WdlFloatType),
    (WdlFileType, "!=", WdlBooleanType),
    (WdlFileType, "<", WdlIntegerType),
    (WdlFileType, "<", WdlFloatType),
    (WdlFileType, "<", WdlStringType),
    (WdlFileType, "<", WdlFileType),
    (WdlFileType, "<", WdlBooleanType),
    (WdlFileType, "<=", WdlIntegerType),
    (WdlFileType, "<=", WdlFloatType),
    (WdlFileType, "<=", WdlBooleanType),
    (WdlFileType, ">", WdlIntegerType),
    (WdlFileType, ">", WdlFloatType),
    (WdlFileType, ">", WdlStringType),
    (WdlFileType, ">", WdlFileType),
    (WdlFileType, ">", WdlBooleanType),
    (WdlFileType, ">=", WdlIntegerType),
    (WdlFileType, ">=", WdlFloatType),
    (WdlFileType, ">=", WdlBooleanType),
    (WdlFileType, "||", WdlIntegerType),
    (WdlFileType, "||", WdlFloatType),
    (WdlFileType, "||", WdlStringType),
    (WdlFileType, "||", WdlFileType),
    (WdlFileType, "||", WdlBooleanType),
    (WdlFileType, "&&", WdlIntegerType),
    (WdlFileType, "&&", WdlFloatType),
    (WdlFileType, "&&", WdlStringType),
    (WdlFileType, "&&", WdlFileType),
    (WdlFileType, "&&", WdlBooleanType),
    (WdlBooleanType, "+", WdlIntegerType),
    (WdlBooleanType, "+", WdlFloatType),
    (WdlBooleanType, "+", WdlStringType),
    (WdlBooleanType, "+", WdlFileType),
    (WdlBooleanType, "+", WdlBooleanType),
    (WdlBooleanType, "-", WdlIntegerType),
    (WdlBooleanType, "-", WdlFloatType),
    (WdlBooleanType, "-", WdlStringType),
    (WdlBooleanType, "-", WdlFileType),
    (WdlBooleanType, "-", WdlBooleanType),
    (WdlBooleanType, "*", WdlIntegerType),
    (WdlBooleanType, "*", WdlFloatType),
    (WdlBooleanType, "*", WdlStringType),
    (WdlBooleanType, "*", WdlFileType),
    (WdlBooleanType, "*", WdlBooleanType),
    (WdlBooleanType, "/", WdlIntegerType),
    (WdlBooleanType, "/", WdlFloatType),
    (WdlBooleanType, "/", WdlStringType),
    (WdlBooleanType, "/", WdlFileType),
    (WdlBooleanType, "/", WdlBooleanType),
    (WdlBooleanType, "%", WdlIntegerType),
    (WdlBooleanType, "%", WdlFloatType),
    (WdlBooleanType, "%", WdlStringType),
    (WdlBooleanType, "%", WdlFileType),
    (WdlBooleanType, "%", WdlBooleanType),
    (WdlBooleanType, "==", WdlIntegerType),
    (WdlBooleanType, "==", WdlFloatType),
    (WdlBooleanType, "==", WdlStringType),
    (WdlBooleanType, "==", WdlFileType),
    (WdlBooleanType, "!=", WdlIntegerType),
    (WdlBooleanType, "!=", WdlFloatType),
    (WdlBooleanType, "!=", WdlStringType),
    (WdlBooleanType, "!=", WdlFileType),
    (WdlBooleanType, "<", WdlIntegerType),
    (WdlBooleanType, "<", WdlFloatType),
    (WdlBooleanType, "<", WdlStringType),
    (WdlBooleanType, "<", WdlFileType),
    (WdlBooleanType, "<=", WdlIntegerType),
    (WdlBooleanType, "<=", WdlFloatType),
    (WdlBooleanType, "<=", WdlStringType),
    (WdlBooleanType, "<=", WdlFileType),
    (WdlBooleanType, ">", WdlIntegerType),
    (WdlBooleanType, ">", WdlFloatType),
    (WdlBooleanType, ">", WdlStringType),
    (WdlBooleanType, ">", WdlFileType),
    (WdlBooleanType, ">=", WdlIntegerType),
    (WdlBooleanType, ">=", WdlFloatType),
    (WdlBooleanType, ">=", WdlStringType),
    (WdlBooleanType, ">=", WdlFileType),
    (WdlBooleanType, "||", WdlIntegerType),
    (WdlBooleanType, "||", WdlFloatType),
    (WdlBooleanType, "||", WdlStringType),
    (WdlBooleanType, "||", WdlFileType),
    (WdlBooleanType, "&&", WdlIntegerType),
    (WdlBooleanType, "&&", WdlFloatType),
    (WdlBooleanType, "&&", WdlStringType),
    (WdlBooleanType, "&&", WdlFileType)
  )

  forAll (validOperations) { (lhs, op, rhs, expectedType) =>
    it should s"validate the output type for the expression: $lhs $op $rhs = $expectedType" in {
      operate(lhs, op, rhs) shouldEqual Success(expectedType)
    }
  }

  forAll (invalidOperations) { (lhs, op, rhs) =>
    it should s"not allow the expression: $lhs $op $rhs" in {
      operate(lhs, op, rhs) should be(a[Failure[_]])
    }
  }

  "Expression Evaluator with Object as LHS" should "Lookup object string attribute" in {
    identifierEval("cgrep.count") shouldEqual WdlIntegerType
  }
  it should "Lookup object integer attribute" in {
    identifierEval("ps.procs") shouldEqual WdlFileType
  }
  it should "Error if key doesn't exist" in {
    identifierEvalError("ps.badkey")
  }
}
