package wdl4s.wom.expression

import wdl4s.wdl.values.WdlValue
import wdl4s.wom.expression.VariableLookupContext.WomOutputResolver
import wdl4s.wom.graph.GraphNode

import scala.util.Try

object VariableLookupContext {
  case class WomNodeAtShard(node: GraphNode, shard: Option[Int])
  type WomOutputResolver = WomNodeAtShard => Try[WdlValue]
}

case class VariableLookupContext(inputs: Map[String, WdlValue], outputResolver: WomOutputResolver)
