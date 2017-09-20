package wdl4s.wom.callable

import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.wdl.types.{WdlOptionalType, WdlType}
import wdl4s.wom.callable.Callable._
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.CallNode.CallNodeAndNewInputs
import wdl4s.wom.graph.{Graph, GraphNodeInputExpression}
import wdl4s.wom.graph.GraphNodePort.OutputPort


trait Callable {
  def name: String

  def graph: ErrorOr[Graph]
  def inputs: List[_ <: InputDefinition]

  def callWithInputs(name: String,
                     portInputs: Map[String, OutputPort],
                     expressionInputs: Set[GraphNodeInputExpression],
                     prefixSeparator: String = "."): ErrorOr[CallNodeAndNewInputs]
}

object Callable {
  sealed trait InputDefinition {
    def name: String
    def womType: WdlType
  }

  final case class OptionalInputDefinition(name: String, womType: WdlOptionalType) extends InputDefinition
  final case class OptionalInputDefinitionWithDefault(name: String, womType: WdlType, default: WomExpression) extends InputDefinition
  final case class RequiredInputDefinition(name: String, womType: WdlType) extends InputDefinition

  case class OutputDefinition(name: String, womType: WdlType, expression: WomExpression)
}
