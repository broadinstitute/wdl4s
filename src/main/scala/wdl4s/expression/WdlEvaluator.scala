package wdl4s.expression

import wdl4s.WdlExpression._
import wdl4s.expression.WdlEvaluator.{ExpressionNotFound, StringMapper, WdlValueMapper}
import wdl4s.values.WdlValue
import wdl4s._

import scala.util.{Failure, Success, Try}

object WdlEvaluator {
  type StringMapper = String => String
  type WdlValueMapper = WdlValue => WdlValue
  case class ExpressionNotFound(declaration: Declaration) extends Exception(
    s"Declaration $declaration does not have an expression and cannot be evaluated."
  )
}

// Can evaluate a wdl value
class WdlEvaluator(defaultLookup: ScopedLookupFunction,
                engineFunctions: WdlFunctions[WdlValue],
                preValueMapper: StringMapper,
                postValueMapper: WdlValueMapper) {

  private def lookup = defaultLookup compose preValueMapper

  // Evaluate a wdl value with the given lookup function, and optionally coerce the result to coerceTo
  private def evaluateValueWith(customLookup: ScopedLookupFunction, wdlValue: WdlValue) = {
    wdlValue match {
      case wdlExpression: WdlExpression => wdlExpression.evaluate(customLookup, engineFunctions) map postValueMapper
      case v: WdlValue => Success(v)
    }
  }

  private def evaluateDeclarationWith(customLookup: ScopedLookupFunction, declaration: Declaration) = {
    val expression = declaration match {
      case resolved: ResolvedDeclaration => Success(resolved.unevaluatedValue)
      case decl if decl.expression.isDefined => Success(declaration.expression.get)
      case _ => Failure(new ExpressionNotFound(declaration))
    }

    expression flatMap { expr =>
      evaluateValueWith(customLookup, expr) flatMap declaration.wdlType.coerceRawValue
    }
  }

  private def mapLookup(values: Map[LocallyQualifiedName, Try[WdlValue]], identifier: String): Try[WdlValue] = {
    values.getOrElse(identifier, Failure(new WdlExpressionException(s"Could not resolve variable '$identifier' as an input parameter")))
  }

  def evaluateValue(wdlValue: WdlValue): Try[WdlValue] = evaluateValueWith(lookup, wdlValue)

  def evaluateDeclaration(declaration: Declaration): Try[WdlValue] = evaluateDeclarationWith(lookup, declaration)

  def evaluateDeclarations(declarations: Seq[Declaration]): Map[Declaration, Try[WdlValue]] = {
    declarations.foldLeft(Map.empty[Declaration, Try[WdlValue]])((evaluatedValues, declaration) => {

      def enhancedLookup(identifier: String) = {
        val mappedDeclaration =  evaluatedValues map { case (k, v) => k.name -> v }
        mapLookup(mappedDeclaration, identifier) getOrElse lookup(identifier)
      }

      evaluatedValues + (declaration -> evaluateDeclarationWith(enhancedLookup, declaration))
    })
  }

  def evaluateValues(wdlValues: Map[LocallyQualifiedName, WdlValue]): Map[LocallyQualifiedName, Try[WdlValue]] = {
    wdlValues.foldLeft(Map.empty[LocallyQualifiedName, Try[WdlValue]])((evaluatedValues, pair) => {

      val (name, wdlValue) = pair
      def enhancedLookup(identifier: String) = mapLookup(evaluatedValues, identifier) getOrElse lookup(identifier)

      evaluatedValues + (name -> evaluateValueWith(enhancedLookup, wdlValue))
    })
  }
}

// Can build an evaluator from engine functions and valueMapper
class WdlEvaluatorBuilder(builder: (WdlFunctions[WdlValue], StringMapper, WdlValueMapper) => WdlEvaluator) {

  def build(functions: WdlFunctions[WdlValue],
            preMapper: StringMapper = identity[String],
            postMapper: WdlValueMapper = identity[WdlValue]) = builder(functions, preMapper, postMapper)
}