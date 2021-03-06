package wom.graph

import wdl.types.{WdlOptionalType, WdlType}
import wom.expression.WomExpression
import wom.graph.GraphNodePort.GraphNodeOutputPort

sealed trait GraphInputNode extends GraphNode {
  def womType: WdlType
  lazy val singleOutputPort: GraphNodeOutputPort = GraphNodeOutputPort(localName, womType, this)

  override val inputPorts: Set[GraphNodePort.InputPort] = Set.empty
  override val outputPorts: Set[GraphNodePort.OutputPort] = Set(singleOutputPort)
}

sealed trait ExternalGraphInputNode extends GraphInputNode {
  /**
    * The fully qualified name should be the same as the one we expect the key in the input file to have.
    * e.g in WDL:
    * workflow.wdl:
    *   workflow w {
    *     String s # "name" = "s", "fullyQualifiedIdentifier" = "w.s"
    *   }
    * 
    * input.json:
    *   {
    *     "w.s": "hi!"
    *   }
    * 
    * e.g in CWL:
    * workflow.cwl:
    *   class: Workflow
    *   inputs:
    *     s: string # "name" = "s", "fullyQualifiedIdentifier" = "s"
    *   
    * inputs.yml:
    *   s: "hi !"
    * 
    */
  
  override lazy val singleOutputPort: GraphNodeOutputPort = GraphNodeOutputPort(identifier, womType, this)
}

final case class RequiredGraphInputNode(override val identifier: WomIdentifier,
                                        womType: WdlType) extends ExternalGraphInputNode

final case class OptionalGraphInputNode(override val identifier: WomIdentifier,
                                        womType: WdlOptionalType) extends ExternalGraphInputNode

// If we want to allow defaults to be "complex" expressions with dependencies we may need to make it an InstantiatedExpression here instead
final case class OptionalGraphInputNodeWithDefault(override val identifier: WomIdentifier,
                                                   womType: WdlType,
                                                   default: WomExpression) extends ExternalGraphInputNode

/**
  * Used to represent an input to any GraphNode's inner graph which is a link to a value somewhere in the outer graph.
  */
final case class OuterGraphInputNode(override val identifier: WomIdentifier, linkToOuterGraph: GraphNodePort.OutputPort) extends GraphInputNode {
  override def womType: WdlType = linkToOuterGraph.womType
}
