package wdl4s.wdl

import wdl4s.parser.WdlParser.Ast
import wdl4s.wdl.AstTools.EnhancedAstNode
import wdl4s.wdl.types.WdlType
import wdl4s.wom.callable.Callable.OutputDefinition

object TaskOutput {
  def apply(ast: Ast, syntaxErrorFormatter: WdlSyntaxErrorFormatter, parent: Option[Scope]): TaskOutput = {
    val wdlType = ast.getAttribute("type").wdlType(syntaxErrorFormatter)
    val name = ast.getAttribute("name").sourceString
    val expression = WdlExpression(ast.getAttribute("expression"))
    val taskOutput = TaskOutput(name, wdlType, expression, ast, parent)
    expression.attachToNode(taskOutput)
    taskOutput
  }
  
  def buildWomOutputDefinition(taskOutput: TaskOutput) = OutputDefinition(taskOutput.unqualifiedName, taskOutput.wdlType, taskOutput.requiredExpression.toWomExpression)
}

final case class TaskOutput(unqualifiedName: String, wdlType: WdlType, requiredExpression: WdlExpression, ast: Ast, override val parent: Option[Scope]) extends Output {
  lazy val womOutputDefinition = TaskOutput.buildWomOutputDefinition(this)
}
