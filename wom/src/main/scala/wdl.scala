
import wdl.exception.OutputVariableLookupException
import wdl.values.WdlValue

import scala.concurrent.Future
import scala.util.{Failure, Try}

package object wdl {
  type WorkflowSource = String
  type WorkflowJson = String
  type WorkflowRawInputs = Map[FullyQualifiedName, Any]
  type WorkflowCoercedInputs = Map[FullyQualifiedName, WdlValue]
  type FullyQualifiedName = String
  type LocallyQualifiedName = String
  type EvaluatedTaskInputs = Map[Declaration, WdlValue]
  type ImportResolver = String => Future[WorkflowSource]
  type OutputResolver = (WdlGraphNode, Option[Int]) => Try[WdlValue]

  val NoOutputResolver: OutputResolver = (node: WdlGraphNode, i: Option[Int]) => Failure(OutputVariableLookupException(node, i))
}
