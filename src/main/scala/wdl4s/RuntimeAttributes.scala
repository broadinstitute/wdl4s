package wdl4s

import wdl4s.AstTools.{AstNodeName, EnhancedAstNode}
import wdl4s.WdlExpression.ScopedLookupFunction
import wdl4s.expression.{NoFunctions, WdlFunctions, WdlStandardLibraryFunctionsType}
import wdl4s.parser.MemoryUnit
import wdl4s.parser.WdlParser.{Ast, AstList, SyntaxError}
import wdl4s.types.{WdlIntegerType, WdlStringType}
import wdl4s.util.TryUtil
import wdl4s.values._

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

case class RuntimeAttributes(attrs: Map[String, WdlExpression], ast: Ast) {
  def evaluate(lookup: ScopedLookupFunction, functions: WdlFunctions[WdlValue]): Map[String, Try[WdlValue]] = {
    attrs map { case (k, v) =>
      k -> v.evaluate(lookup, functions)
    }
  }
}

object RuntimeAttributes {
  def apply(ast: Ast): RuntimeAttributes = {
    val asts = ast.findAsts(AstNodeName.Runtime)
    if (asts.size > 1) throw new UnsupportedOperationException("Only one runtime block may be defined per task")
    val kvPairAsts = asts.headOption.map(_.getAttribute("map").asInstanceOf[AstList].asScala.toVector.map(_.asInstanceOf[Ast]))
    val kvPairs = kvPairAsts match {
      case Some(vector) => vector.map(processRuntimeAttribute).toMap
      case None => Map.empty[String, WdlExpression]
    }
    RuntimeAttributes(kvPairs, ast)
  }

  private def processRuntimeAttribute(ast: Ast): (String, WdlExpression) = {
    val key = ast.getAttribute("key").sourceString
    val expression = WdlExpression(ast.getAttribute("value"))
    key -> expression
  }
}
