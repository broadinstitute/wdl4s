package wdl4s

import com.typesafe.scalalogging.LazyLogging
import wdl4s.AstTools.EnhancedAstNode
import wdl4s.parser.WdlParser.Ast
import wdl4s.types.WdlType

object TaskOutput extends LazyLogging {
  def apply(ast: Ast, syntaxErrorFormatter: WdlSyntaxErrorFormatter): TaskOutput = {
    val wdlType = ast.getAttribute("type").wdlType(syntaxErrorFormatter)
    val name = ast.getAttribute("var").sourceString
    val expression = WdlExpression(ast.getAttribute("expression"))
    TaskOutput(name, wdlType, expression)
  }
}

case class TaskOutput(name: String, wdlType: WdlType, requiredExpression: WdlExpression) extends Declaration {
  override val postfixQuantifier = None
  override val expression = Option(requiredExpression)
}
