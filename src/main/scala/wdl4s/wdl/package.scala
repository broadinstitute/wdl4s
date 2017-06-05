package wdl4s

import wdl4s.wdl.WdlGraphNode
import wdl4s.wdl.exception.OutputVariableLookupException
import wdl4s.wdl.values.WdlValue

import scala.util.{Failure, Try}

package object wdl {
  type WdlSource = String
  type WdlJson = String
  type WorkflowRawInputs = Map[FullyQualifiedName, Any]
  type WorkflowCoercedInputs = Map[FullyQualifiedName, WdlValue]
  type FullyQualifiedName = String
  type LocallyQualifiedName = String
  type EvaluatedTaskInputs = Map[Declaration, WdlValue]
  type ImportResolver = String => WdlSource
  type OutputResolver = (WdlGraphNode, Option[Int]) => Try[WdlValue]
  
  val NoOutputResolver: OutputResolver = (node: WdlGraphNode, i: Option[Int]) => Failure(new OutputVariableLookupException(node, i))

  trait TsvSerializable {
    def tsvSerialize: Try[String]
  }

}
