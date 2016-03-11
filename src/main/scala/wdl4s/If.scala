package wdl4s

import wdl4s.AstTools.EnhancedAstNode
import wdl4s.parser.WdlParser.Ast

object If {
  val FQNIdentifier = "$if"

  /**
    * @param index Index of the if block. The index is computed during tree generation to reflect WDL scope structure.
    */
  def apply(ast: Ast, index: Int): If = {
    new If(index, WdlExpression(ast.getAttribute("expression")), ast)
  }
}

/**
  * Represents an If block in WDL
  *
  * @param index Index of the if block. The index is computed during tree generation to reflect WDL scope structure.
  * @param condition WDL Expression representing the condition in which to execute this If-block
  */
case class If(index: Int, condition: WdlExpression, ast: Ast) extends Scope with GraphNode {
  val unqualifiedName = s"${If.FQNIdentifier}_$index"
  override def appearsInFqn = false

  // TODO: sfrazer: copied from Scatter
  lazy val upstream: Set[Scope with GraphNode] = {
    (for {
      variable <- condition.variableReferences
      node <- resolveVariable(variable.sourceString)
      if node.fullyQualifiedNameWithIndexScopes != fullyQualifiedNameWithIndexScopes
    } yield node).toSet
  }

  lazy val downstream: Set[Scope with GraphNode] = {
    for {
      node <- namespace.descendants.collect({ case n: GraphNode => n }).filter(_.fullyQualifiedName != fullyQualifiedName)
      if node.upstream.contains(this)
    } yield node
  }
}

