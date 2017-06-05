package wdl4s.wom.expression

import wdl4s.wdl.types.WdlType
import wdl4s.wdl.values.WdlValue

sealed trait Expression {
  def requiredVariables: Set[String]
  def evaluate(variableValues: Map[String, WdlValue])
  def womType: WdlType
}

case class PlaceholderExpression(womType: WdlType) extends Expression {
  override def requiredVariables: Set[String] = ???
  override def evaluate(variableValues: Map[String, WdlValue]) = ???
}