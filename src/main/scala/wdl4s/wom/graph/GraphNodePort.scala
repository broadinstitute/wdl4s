package wdl4s.wom.graph

import wdl4s.wdl.types.{WdlArrayType, WdlOptionalType, WdlType}

sealed trait GraphNodePort {
  def name: String
  def womType: WdlType

  /**
    * The GraphNode which owns this port.
    */
  def graphNode: GraphNode
}

object GraphNodePort {

  // TODO: It'd be really cool if these could be typed (eg InputPort[WdlString], OutputPort[WdlInteger] but
  // TODO: we'd have to think about coercion... maybe some sort of implicit CoercionSocket[WdlString, WdlInteger]...?
  sealed trait InputPort extends GraphNodePort {
    def upstream: OutputPort
  }

  sealed trait OutputPort extends GraphNodePort {

    // TODO: Might end up wanting a backwards link to the InputPorts that use this (eg def downstream: Set[InputPort])?
  }

  final case class ConnectedInputPort(name: String, womType: WdlType, upstreamPort: OutputPort, g: () => GraphNode) extends InputPort {
    override def upstream = upstreamPort
    override lazy val graphNode: GraphNode = g()
  }

  final case class GraphOutputNodePort(upstreamPort: OutputPort, graphOutputNode: GraphOutputNode) extends InputPort {
    override lazy val name = graphOutputNode.name
    override lazy val womType = graphOutputNode.womType
    override val graphNode = graphOutputNode

    override def upstream: OutputPort = upstreamPort
  }

  /**
    * For any graph node that uses a declarations to produce outputs (e.g. call, declaration):
    */
  final case class DeclarationOutputPort(name: String, womType: WdlType, graphNode: GraphNode) extends OutputPort

  // TODO: For these next two, the graphNode should be a ScatterNode and IfNode respectively (once those exist):
  /**
    * Represents the gathered output from a call/declaration in a scatter.
    */
  final case class ScatterGathererPort(name: String, womType: WdlArrayType, outputToGather: OutputPort, graphNode: GraphNode) extends OutputPort
  final case class ConditionalOutputPort(name: String, womType: WdlOptionalType, outputToExpose: OutputPort, graphNode: GraphNode) extends OutputPort

  /**
    * For workflow inputs to provide values as a source:
    */
  final case class GraphInputNodePort(graphInputNode: GraphInputNode) extends OutputPort {
    override val name = graphInputNode.name
    override val womType = graphInputNode.womType
    override val graphNode = graphInputNode
  }
}
