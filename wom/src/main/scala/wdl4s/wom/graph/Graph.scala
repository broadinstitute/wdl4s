package wdl4s.wom.graph

import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.validated._
import lenthall.collections.EnhancedCollections._
import lenthall.validation.ErrorOr.{ErrorOr, _}
import lenthall.validation.Validation._
import wdl4s.wdl.WorkflowRawInputs
import wdl4s.wdl.values.WdlValue
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.Graph.ResolvedWorkflowInput
import wdl4s.wom.graph.GraphNodePort.{InputPort, OutputPort}

/**
  * A sealed set of graph nodes.
  */
final case class Graph private(nodes: Set[GraphNode]) {
  lazy val inputNodes: Set[GraphInputNode] = nodes.filterByType[GraphInputNode]
  lazy val outputNodes: Set[GraphOutputNode] = nodes.filterByType[GraphOutputNode]
  lazy val calls: Set[CallNode] = nodes.filterByType[CallNode]
  lazy val scatters: Set[ScatterNode] = nodes.filterByType[ScatterNode]

  def outputByName(name: String): Option[GraphOutputNode] = outputNodes.find(_.name == name)

  /**
    * Maps GraphInputNode to their final value / expression using workflow inputs. Validate all required inputs are satisfied.
    * @param inputsMapping workflow inputs in the form of Map[FQN, Any]
    * @return validated mappings from GraphInputPort to ResolvedWorkflowInput, which can be a WdlValue or an expression
    */
    // TODO we shouldn't need the prefix with correct FQNs
  def validateWorkflowInputs(inputsMapping: WorkflowRawInputs, prefix: String = ""): ErrorOr[Map[OutputPort, ResolvedWorkflowInput]] = {

    def coerceRawValue(value: Any, gin: ExternalGraphInputNode): ErrorOr[WdlValue] = {
      gin.womType.coerceRawValue(value).toErrorOr
    }
    
    def fromInputMapping(gin: ExternalGraphInputNode): Option[ErrorOr[ResolvedWorkflowInput]] = {
      inputsMapping.get(s"$prefix${gin.name}").map(coerceRawValue(_, gin).map(Left.apply))
    }

    def fallBack(gin: ExternalGraphInputNode): ErrorOr[ResolvedWorkflowInput] = gin match {
      case required: RequiredGraphInputNode => s"Cannot find an input value for ${required.name}".invalidNel
      case optionalWithDefault: OptionalGraphInputNodeWithDefault => Right(optionalWithDefault.default).validNel
      case optional: OptionalGraphInputNode => Left(optional.womType.none).validNel
    }

    nodes.collect({
      case gin: ExternalGraphInputNode => 
        // The compiler needs the type ascription for some reason
        (gin.singleOutputPort: OutputPort) -> fromInputMapping(gin).getOrElse(fallBack(gin))
    }).toMap.sequence
  }
}

object Graph {

  type ResolvedWorkflowInput = Either[WdlValue, WomExpression]

  /**
    * Checks that every input port for every node in the graph references an upstream node that is also in the graph.
    * Assuming it validates, construct the Graph case class.
    */
  def validateAndConstruct(nodes: Set[GraphNode]): ErrorOr[Graph] = {

    def boolToErrorOr(bool: Boolean, msg: => String): ErrorOr[Unit] = if (bool) ().validNel else msg.invalidNel

    def upstreamNodeInGraph(port: InputPort): ErrorOr[Unit] = {
      val upstreamOutputPort = port.upstream
      boolToErrorOr(nodes.exists(_ eq upstreamOutputPort.graphNode), s"The input link ${port.name} on ${port.graphNode.name} is linked to a node outside the graph set (${upstreamOutputPort.name})")
    }

    def portProperlyEmbedded(port: GraphNodePort, portFinder: GraphNode => Set[_ <: GraphNodePort]): ErrorOr[Unit] = {
      boolToErrorOr(portFinder(port.graphNode).exists(_ eq port), s"The port ${port.name} thinks it belongs to a Node (${port.graphNode}), but that Node doesn't think it owns it.")
    }

    def goodLink(port: InputPort): ErrorOr[Unit] = {
      val upstreamNodeValidation = upstreamNodeInGraph(port)
      val inputPortEmbeddedValidation = portProperlyEmbedded(port, _.inputPorts)
      val upstreamPortEmbeddedValidation = portProperlyEmbedded(port.upstream, _.outputPorts)

      (upstreamNodeValidation, inputPortEmbeddedValidation, upstreamPortEmbeddedValidation).tupled.void
    }

    def validateNode(node: GraphNode): ErrorOr[Unit] = {
      node.inputPorts.toList.traverse(goodLink).void
    }

    nodes.toList.traverse(validateNode).map(_ => Graph(nodes))
  }
}
