package wdl4s

import wdl4s.parser.WdlParser.{Ast, Terminal}
import wdl4s.AstTools.EnhancedAstNode

object Scatter {
  val FQNIdentifier = "$scatter"

  /**
   * @param index Index of the scatter block. The index is computed during tree generation to reflect wdl scatter blocks structure.
   */
  def apply(ast: Ast, index: Int): Scatter = {
    val item = ast.getAttribute("item").asInstanceOf[Terminal].getSourceString
    new Scatter(index, item, WdlExpression(ast.getAttribute("collection")))
  }
}

/**
 * Scatter class.
 * @param index Index of the scatter block. The index is computed during tree generation to reflect wdl scatter blocks structure.
 * @param item Item which this block is scattering over
 * @param collection Wdl Expression corresponding to the collection this scatter is looping through
 */
case class Scatter(index: Int, item: String, collection: WdlExpression) extends Scope with GraphNode {
  val unqualifiedName = s"${Scatter.FQNIdentifier}_$index"
  override def appearsInFqn = false

  lazy val upstream: Set[Scope with GraphNode] = {
    (for {
      variable <- collection.variableReferences
      node <- resolveVariable(variable.sourceString)
      if node.fullyQualifiedNameWithIndexScopes != fullyQualifiedNameWithIndexScopes
    } yield node).toSet
  }

  lazy val downstream: Set[Scope with GraphNode] = {
    for {
      node <- namespace.descendants.collect({ case n: GraphNode => n }).filter(_.fullyQualifiedName != fullyQualifiedName).toSet
      if node.upstream.contains(this)
    } yield node
  }
}
