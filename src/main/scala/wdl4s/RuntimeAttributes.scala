package wdl4s

import wdl4s.AstTools.{AstNodeName, EnhancedAstNode}
import wdl4s.WdlExpression.ScopedLookupFunction
import wdl4s.expression.{NoFunctions, WdlFunctions, WdlStandardLibraryFunctionsType}
import wdl4s.parser.WdlParser.{Ast, AstList, SyntaxError}
import wdl4s.parser.MemoryUnit
import wdl4s.types.{WdlIntegerType, WdlStringType}
import wdl4s.util.TryUtil
import wdl4s.values._

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scalaz.Scalaz._
import scalaz.ValidationNel

case class RuntimeAttributes(attrs: Map[String, WdlExpression]) {
  import RuntimeAttributes._
  def evaluate(lookup: ScopedLookupFunction, functions: WdlFunctions[WdlValue]): Map[String, Try[WdlValue]] = {
    attrs map { case (k, v) =>
      val evaluated = v.evaluate(lookup, functions).flatMap(validateRuntimeValue(k, _))
      k -> evaluated
    }
  }

  private def validateRuntimeValue(key: String, value: WdlValue): Try[WdlValue] = {
    key match {
      case "memory" => validationNelToTry(validateMemoryValue(value))
      case _ => Success(value)
    }
  }
}

object RuntimeAttributes {
  def apply(ast: Ast, declarations: Seq[Declaration]): RuntimeAttributes = {
    val asts = ast.findAsts(AstNodeName.Runtime)
    if (asts.size > 1) throw new UnsupportedOperationException("Only one runtime block may be defined per task")
    val kvPairAsts = asts.headOption.map(_.getAttribute("map").asInstanceOf[AstList].asScala.toVector.map(_.asInstanceOf[Ast]))
    val runtimeAttributeMap = kvPairAsts match {
      case Some(list) => list.map(ast => processRuntimeAttribute(ast, declarations)).toMap
      case None => Map.empty[String, Try[WdlExpression]]
    }

    // .get below to throw the exception if validation didn't pass
    TryUtil.sequenceMap(runtimeAttributeMap).map(RuntimeAttributes(_)).get
  }

  private def processRuntimeAttribute(ast: Ast, declarations: Seq[Declaration]): (String, Try[WdlExpression]) = {
    val key = ast.getAttribute("key").sourceString
    val expression = WdlExpression(ast.getAttribute("value"))
    val validatedExpression = key match {
      case "memory" => validateMemoryExpression(expression, declarations)
      case _ => Success(expression)
    }
    key -> validatedExpression
  }

  private def validateMemoryExpression(expression: WdlExpression, declarations: Seq[Declaration]): Try[WdlExpression] = {
    // .get below because lookup functions should throw exceptions if they could not lookup the variable
    def lookupType(n: String) = declarations.find(_.name == n).map(_.wdlType).get
    expression.evaluateType(lookupType, new WdlStandardLibraryFunctionsType) match {
      case Success(wdlType) =>
        if (!Seq(WdlIntegerType, WdlStringType).contains(wdlType)) {
          Failure(new SyntaxError("blah"))
        } else {
          expression.evaluate(NoLookup, NoFunctions) match {
            case Success(value) => validateMemoryValue(value) match {
              case scalaz.Success(x) => Success(expression)
              case scalaz.Failure(x) => Failure(new UnsupportedOperationException(x.list.mkString("\n")))
            }
            case Failure(ex) =>
              // This just means that the expression isn't statically evaluatable
              Success(expression)
          }
        }
      case Failure(ex) => Failure(ex)
    }
  }

  private def validateMemoryValue(value: WdlValue): ValidationNel[String, WdlValue] = {
    value match {
      case i: WdlInteger => value.successNel
      case s: WdlString => validateMemStringInGb(s.valueString)
    }
  }

  private def validateMemStringInGb(mem: String): ValidationNel[String, WdlValue] = {
    val memoryPattern = """(\d+)\s*(\w+)""".r
    mem match {
      case memoryPattern(amountString, unitString) =>
        val amount = amountString.parseLong leftMap { _.getMessage } toValidationNel
        val unit = validateMemoryUnit(unitString)
        (amount |@| unit) { (a, u) =>
          WdlString(new MemorySize(a, u).to(MemoryUnit.GB).toString)
        }
      case _ => s"$mem should be of the form X Unit where X is a number, e.g. 8 GB".failureNel
    }
  }

  private def validateMemoryUnit(unit: String): ValidationNel[String, MemoryUnit] = {
    MemoryUnit.values find { _.suffixes.contains(unit) } match {
      case Some(s) => s.successNel
      case None => s"$unit is an invalid memory unit".failureNel
    }
  }

  private def validationNelToTry(nel: ValidationNel[String, WdlValue]): Try[WdlValue] = {
    nel match {
      case scalaz.Success(x) => scala.util.Success(x)
      case scalaz.Failure(n) => scala.util.Failure(new UnsupportedOperationException(n.list.mkString("\n")))
    }
  }
}
