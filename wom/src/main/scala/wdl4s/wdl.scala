package wdl4s

import wdl4s.wdl.exception.OutputVariableLookupException
import wdl4s.wdl.values.WdlValue

import scala.util.{Failure, Try}

package object wdl {
  type WorkflowSource = String
  type WorkflowJson = String
  type WorkflowRawInputs = Map[FullyQualifiedName, Any]
  type WorkflowCoercedInputs = Map[FullyQualifiedName, WdlValue]
  type FullyQualifiedName = String
  type LocallyQualifiedName = String
  type EvaluatedTaskInputs = Map[Declaration, WdlValue]
  type ImportResolver = String => WorkflowSource
  
  case class WdlNodeAtShard(node: WdlGraphNode, shard: Option[Int])
  type WdlOutputResolver = WdlNodeAtShard => Try[WdlValue]

  val NoOutputResolver: WdlOutputResolver = (nodeAtShard: WdlNodeAtShard) => Failure(OutputVariableLookupException(nodeAtShard.node, nodeAtShard.shard))
}
