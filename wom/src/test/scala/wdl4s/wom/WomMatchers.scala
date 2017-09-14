package wdl4s.wom

import org.scalactic.Equality
import org.scalatest.matchers._
import wdl4s.wom.graph.GraphNode

trait WomMatchers {

  class GraphNodeReferenceMatcher(expectedNode: GraphNode) extends Matcher[GraphNode] {

    def apply(left: GraphNode) = {
      MatchResult(
        left eq expectedNode,
        s"GraphNode $left was not the same object as $expectedNode",
        s"GraphNode $left was the same object as $expectedNode"
      )
    }
  }

  def beTheSameAs(expectedNode: GraphNode) = new GraphNodeReferenceMatcher(expectedNode)

  implicit val graphNodeReferenceEquality = new Equality[GraphNode] {
    override def areEqual(left: GraphNode, right: Any): Boolean = right match {
      case node: GraphNode => left eq node
      case _ => false
    }
  }
}

object WomMatchers extends WomMatchers