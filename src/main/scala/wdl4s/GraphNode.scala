package wdl4s

import wdl4s.parser.WdlParser.Terminal
import AstTools.EnhancedAstNode

sealed trait GraphNode extends Scope {

  final lazy val upstreamAncestry: Set[GraphNode] = GraphNode.calculateUpstreamAncestry(Set.empty, this)

  def referencedNodes: Iterable[GraphNode]

  final lazy val upstream: Set[GraphNode] = {

    if (this.fullyQualifiedName.equals("test1.summary")) {
      val x = 5
    }

    // If we are inside the scope of another graph node (i.e. via document element ancestry), then
    // that is also upstream of us.
    val closestScopedAncestor = ancestry.collectFirst({
      case ancestor: GraphNode => ancestor
    })

    (referencedNodes ++ closestScopedAncestor.toSeq).toSet
  }

  final lazy val downstream: Set[GraphNode] = {
    for {
      node <- namespace.descendants.collect({
        case n: GraphNode if n.fullyQualifiedName != fullyQualifiedName => n
      })
      if node.upstream.contains(this)
    } yield node
  }
}

object GraphNode {
  private def calculateUpstreamAncestry(currentSet: Set[GraphNode], graphNode: GraphNode): Set[GraphNode] = {

    if (graphNode.fullyQualifiedName.equals("w.$scatter_0")) {
      val x = 5
    }

    val setWithUpstream = currentSet ++ graphNode.upstream
    val updatesNeeded = graphNode.upstream -- currentSet
    updatesNeeded.foldLeft(setWithUpstream)(calculateUpstreamAncestry)
  }
}

trait GraphNodeWithUpstreamReferences extends GraphNode {
  def upstreamReferences: Iterable[Terminal]

  // If we have variable reference to other graph nodes, then they are upstream from us.
  override final def referencedNodes = for {
      variable <- upstreamReferences
      node <- resolveVariable(variable.sourceString)
      if node.fullyQualifiedNameWithIndexScopes != fullyQualifiedNameWithIndexScopes
    } yield node
}

trait GraphNodeWithInputs extends GraphNode {
  def inputMappings: Map[String, WdlExpression]

  override final def referencedNodes = for {
    expr <- inputMappings.values
    variable <- expr.variableReferences
    node <- parent.flatMap{ _.resolveVariable(variable.sourceString) }
  } yield node
}