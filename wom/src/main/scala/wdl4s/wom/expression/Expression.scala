package wdl4s.wom.expression

import wdl4s.wdl.types.WdlType
import wdl4s.wdl.values.WdlValue
import wdl4s.wom.graph.{GraphNode, GraphNodePort}

import scala.concurrent.Future

sealed trait Expression extends GraphNode {
  def name: String
  def evaluate(variableLookupContext: VariableLookupContext, ioFunctions: IoFunctions): Future[WdlValue]
}

case class PlaceholderExpression(womType: WdlType) extends Expression {
  override def evaluate(variableLookupContext: VariableLookupContext, ioFunctions: IoFunctions): Future[WdlValue] = ???
  override def inputPorts: Set[GraphNodePort.InputPort] = ???
  override def outputPorts: Set[GraphNodePort.OutputPort] = ???
  override def name: String = ???
}

abstract class WomExpression extends Expression