package wdl4s.wom.graph

import wdl4s.wdl.types.{WdlOptionalType, WdlType}
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.GraphNodePort.GraphNodeOutputPort

sealed trait GraphInputNode extends GraphNode {
  def womType: WdlType
  val singleOutputPort: GraphNodeOutputPort = GraphNodeOutputPort(name, womType, this)

  override val inputPorts: Set[GraphNodePort.InputPort] = Set.empty
  override val outputPorts: Set[GraphNodePort.OutputPort] = Set(singleOutputPort)
}

sealed trait ExternalGraphInputNode extends GraphInputNode {
  /**
    * Name of this node that is to be looked for in external the input map available at runtime.
    * e.g in WDL:
    * workflow w {
    *   String s # "name" = "s", "fullyQualifiedIdentifier" = "w.s"
    * }
    * 
    * e.g in CWL:
    * class: Workflow
    * inputs:
    *   s: string # "name" = "s", "fullyQualifiedIdentifier" = "s"
    */
  def fullyQualifiedIdentifier: String
}

object RequiredGraphInputNode {
  def apply(name: String, womType: WdlType): RequiredGraphInputNode = {
    RequiredGraphInputNode(name, womType, name)
  }
}

final case class RequiredGraphInputNode(override val name: String,
                                        womType: WdlType,
                                        fullyQualifiedIdentifier: String) extends ExternalGraphInputNode

object OptionalGraphInputNode {
  def apply(name: String, womType: WdlOptionalType): OptionalGraphInputNode = {
    OptionalGraphInputNode(name, womType, name)
  }
}

final case class OptionalGraphInputNode(override val name: String,
                                        womType: WdlOptionalType,
                                        fullyQualifiedIdentifier: String) extends ExternalGraphInputNode

object OptionalGraphInputNodeWithDefault {
  def apply(name: String, womType: WdlType, default: WomExpression): OptionalGraphInputNodeWithDefault = {
    OptionalGraphInputNodeWithDefault(name, womType, default, name)
  }
}

// If we want to allow defaults to be "complex" expressions with dependencies we may need to make it an InstantiatedExpression here instead
final case class OptionalGraphInputNodeWithDefault(override val name: String,
                                                   womType: WdlType,
                                                   default: WomExpression,
                                                   fullyQualifiedIdentifier: String) extends ExternalGraphInputNode

/**
  * Used to represent an input to any GraphNode's inner graph which is a link to a value somewhere in the outer graph.
  */
final case class OuterGraphInputNode(override val name: String, linkToOuterGraph: GraphNodePort.OutputPort) extends GraphInputNode {
  override def womType: WdlType = linkToOuterGraph.womType
}
