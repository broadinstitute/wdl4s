package wdl4s.wom.expression

import wdl4s.wdl.types.WdlType
import wdl4s.wdl.values.WdlValue
import wdl4s.wom.graph.GraphNodePort.GraphNodeOutputPort

import scala.concurrent.Future

sealed trait Expression {
  /**
    * Returns the mapping from variables referenced in the expression to the corresponding output ports from nodes in the graph.
    * Can be empty.
    */
  def referencedVariables: Map[String, GraphNodeOutputPort]

  /**
    * Evaluates the expression given a context and functions.
    */
  def evaluate(variableLookupContext: VariableLookupContext, ioFunctions: IoFunctions): Future[WdlValue]
}

case class PlaceholderExpression(womType: WdlType) extends Expression {
  override def evaluate(variableLookupContext: VariableLookupContext, ioFunctions: IoFunctions): Future[WdlValue] = ???
  override def referencedVariables: Map[String, GraphNodeOutputPort] = ???
}

abstract class WomExpression extends Expression