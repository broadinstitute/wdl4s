package wdl4s.command

import wdl4s.AstTools.EnhancedAstNode
import wdl4s._
import wdl4s.expression.WdlFunctions
import wdl4s.values._
import wdl4s.parser.WdlParser.{Terminal, SyntaxError, Ast}
import scala.language.postfixOps

import scala.util.{Failure, Success}

object ParameterCommandPart {
  def apply(ast: Ast, wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): ParameterCommandPart = {
    val attributes = ast.getAttribute("attributes").astListAsVector map { a =>
      val ast = a.asInstanceOf[Ast]
      (ast.getAttribute("key").sourceString, ast.getAttribute("value").sourceString)
    } toMap
    val expression = WdlExpression(ast.getAttribute("expr"))
    if ((attributes.contains("true") && !attributes.contains("false")) || (attributes.contains("false") && !attributes.contains("true"))) {
      // .head because we can't get here without there being at least one attribute
      val firstAttr = ast.getAttribute("attributes").astListAsVector.head.asInstanceOf[Ast].getAttribute("key").asInstanceOf[Terminal]
      throw new SyntaxError(wdlSyntaxErrorFormatter.trueAndFalseAttributesAreRequired(firstAttr))
    }
    new ParameterCommandPart(attributes, expression)
  }
}

case class ParameterCommandPart(attributes: Map[String, String], expression: WdlExpression) extends CommandPart {
  def attributesToString: String = if (attributes.nonEmpty) attributes.map({case (k,v) => s"$k=${WdlString(v).toWdlString}"}).mkString(" ") + " " else ""
  override def toString: String = "${" + s"$attributesToString${expression.toWdlString}" + "}"

  override def instantiate(declarations: Seq[Declaration], inputsMap: EvaluatedTaskInputs, functions: WdlFunctions[WdlValue], valueMapper: (WdlValue) => WdlValue): String = {
    val inputs = inputsMap map { case (d, v) => d.unqualifiedName -> v }
    val lookup = (s: String) => inputs.getOrElse(s, throw new VariableNotFoundException(s))

    val value = expression.evaluate(lookup, functions) match {
      case Success(v) => v
      case Failure(f) => f match {
        case v: VariableNotFoundException => declarations.find(_.unqualifiedName == v.variable) match {
          /* Allow an expression to fail evaluation if one of the variables that it requires is optional (the type has ? after it, e.g. String?) */
          case Some(d) if d.postfixQuantifier.contains(Declaration.OptionalPostfixQuantifier) => if (attributes.contains("default")) WdlString(attributes.get("default").head) else WdlString("")
          case Some(d) => throw new UnsupportedOperationException(s"Parameter ${v.variable} is required, but no value is specified")
          case None => throw new UnsupportedOperationException(s"Could not find declaration for ${v.variable}")
        }
        case e => throw new UnsupportedOperationException(s"Could not evaluate expression: ${expression.toWdlString}", e)
      }
    }

    valueMapper(value) match {
      case b: WdlBoolean if attributes.contains("true") && attributes.contains("false") => if (b.value) attributes.get("true").head else attributes.get("false").head
      case p: WdlPrimitive => p.valueString
      case a: WdlArray if attributes.contains("sep") => a.value.map(_.valueString).mkString(attributes.get("sep").head)
      case a: WdlArray => throw new UnsupportedOperationException(s"Expression '${expression.toString}' evaluated to an Array but no 'sep' was specified")
      case _ => throw new UnsupportedOperationException(s"Could not string-ify value: $value")
    }
  }
}
