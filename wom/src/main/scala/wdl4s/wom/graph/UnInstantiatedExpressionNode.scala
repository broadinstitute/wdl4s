package wdl4s.wom.graph

import wdl4s.wdl.types.WdlType
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.GraphNodePort.{GraphNodeOutputPort, InputPort}


/**
  * Used to hold an un-instantiated expression. This is useful for a task declaration for example
  * for which expression inputs are not explicitly mapped to other output ports but are instead derived from preceding
  * inputs.
  *
  * e.g:
  *
  * task t {
  *   String r
  *   String s = r + "hello!"
  *   ...
  * }
  *
  * workflow w {
  *   call t { input: r = ... }
  * }
  */
final case class UnInstantiatedExpressionNode(override val name: String, womExpression: WomExpression, womType: WdlType) extends GraphNode {
  val singleExpressionOutputPort = GraphNodeOutputPort(name, womType, this)

  override val inputPorts = Set.empty[InputPort]
  override val outputPorts: Set[GraphNodePort.OutputPort] = Set(singleExpressionOutputPort)
}
