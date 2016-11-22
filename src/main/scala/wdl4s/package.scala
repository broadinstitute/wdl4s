import wdl4s.values.{WdlCallOutputsObject, WdlValue}

import scala.util.{Failure, Try}

package object wdl4s {
  type WdlSource = String
  type WdlJson = String
  type WorkflowRawInputs = Map[FullyQualifiedName, Any]
  type WorkflowCoercedInputs = Map[FullyQualifiedName, WdlValue]
  type FullyQualifiedName = String
  type LocallyQualifiedName = String
  type EvaluatedTaskInputs = Map[Declaration, WdlValue]
  type ImportResolver = String => WdlSource
  type OutputResolver = (Call, Option[Int]) => Try[WdlCallOutputsObject]
  
  class OutputVariableLookupException(call: Call, index: Option[Int]) extends VariableLookupException(s"Could not find outputs for call ${call.fullyQualifiedName} at index $index")
  val NoOutputResolver: OutputResolver = (c: Call, i: Option[Int]) => Failure(new OutputVariableLookupException(c, i))

  trait TsvSerializable {
    def tsvSerialize: Try[String]
  }

}
