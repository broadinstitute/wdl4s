package wdl4s

import wdl4s.AstTools.{AstNodeName, EnhancedAstNode}
import wdl4s.parser.WdlParser.Ast
//
//object Output {
//  def apply(ast: Ast, syntaxErrorFormatter: WdlSyntaxErrorFormatter, parent: Option[Scope]): Output = {
//    val wdlType = ast.getAttribute("type").wdlType(syntaxErrorFormatter)
//    val name = ast.getAttribute("name").sourceString
//    val expression = WdlExpression(ast.getAttribute("expression"))
//    ast.getName match {
//      case AstNodeName.WorkflowOutputDeclaration => WorkflowOutput(name, wdlType, expression, ast, parent)
//      case AstNodeName.Output => TaskOutput(name, wdlType, expression, ast, parent)
//    }
//    TaskOutput(name, wdlType, expression, ast, parent)
//  }
//}

trait Output extends DeclarationInterface {
  def requiredExpression: WdlExpression
  
  override val postfixQuantifier = None
  override val expression = Option(requiredExpression)
}
