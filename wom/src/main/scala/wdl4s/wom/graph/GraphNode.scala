package wdl4s.wom.graph

import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.wom.graph.GraphNode._
import wdl4s.wom.graph.GraphNodePort.{ConnectedInputPort, InputPort, OutputPort}
import cats.implicits._
import wdl4s.wom.callable.Callable
import wdl4s.wom.callable.Callable.{OptionalInputDefinition, OptionalInputDefinitionWithDefault, RequiredInputDefinition}

trait GraphNode {

  /**
    * Inputs that must be available before this graph node can be run.
    */
  def inputPorts: Set[GraphNodePort.InputPort]

  /**
    * Outputs that are generated by this GraphNode
    */
  def outputPorts: Set[GraphNodePort.OutputPort]

  def outputByName(name: String): ErrorOr[GraphNodePort.OutputPort] = {
    outputPorts.find(_.name == name) match {
      case Some(port) => port.validNel
      case None => s"No such output: $name".invalidNel
    }
  }

  /**
    * The set of all graph nodes which are (transitively) upstream from this one.
    */
  lazy val upstreamAncestry = calculateUpstreamAncestry(Set.empty, this)
  lazy val upstream: Set[GraphNode] = inputPorts.map(_.upstream.graphNode)
}

object GraphNode {
  // A recursive traversal with a fancy trick to avoid double-counting:
  private def calculateUpstreamAncestry(currentSet: Set[GraphNode], graphNode: GraphNode): Set[GraphNode] = {
    val setWithUpstream = currentSet ++ graphNode.upstream
    val updatesNeeded = graphNode.upstream -- currentSet
    updatesNeeded.foldLeft(setWithUpstream)(calculateUpstreamAncestry)
  }

  def inputPortNamesMatch(required: Set[InputPort], provided: Set[InputPort]): ErrorOr[Unit] = {
    def requiredInputFound(r: InputPort): ErrorOr[Unit] = provided.find(_.name == r.name) match {
      case Some(p) => if (r.womType.isCoerceableFrom(p.womType)) ().validNel else s"Cannot link a ${p.womType.toWdlString} to the input ${r.name}: ${r.womType}".invalidNel
      case None => s"The required input ${r.name}: ${r.womType.toWdlString} was not provided.".invalidNel
    }

    required.toList.traverse(requiredInputFound).void
  }

  /**
    * Allows a level of indirection, so that GraphNodePorts can be constructed before their associated GraphNode is
    * constructed. If used, the _graphNode must be set before anything tries to apply 'get'.
    */
  private[graph] class GraphNodeSetter {
    var _graphNode: GraphNode = _
    private def getGraphNode = _graphNode
    def get: Unit => GraphNode = _ => getGraphNode
  }

  private[graph] case class LinkedInputPort(newInputPort: InputPort, newGraphInput: Option[GraphInputNode])

  /**
    * Attempts to create an InputPort from the input definition, linked to one of the inputMapping entries.
    * If it fails to find an appropriate input, it will create a new GraphInputNode instead.
    * @param inputPrefix If the input port should have a prefix to its name as well as the inputDefinition's name.
    * @param inputMapping Set of inputs available to this linker.
    * @param callNodeRef We construct the input port with a lazy GraphNode reference. At some point this function
    *                    will be called by the new InputPort to link to its parent GraphNode.
    *                    (we do this because the GraphNode might not be constructed yet, since it itself needs a set
    *                    of InputPorts for construction, and that's what we're making here...)
    * @param inputDefinition The input to create an input port for.
    * @return The new input port, and a GraphInputNode if we needed to make one. Wrapped in a case class.
    */
  private[graph] def linkInputPort(inputPrefix: String, inputMapping: Map[String, OutputPort], callNodeRef: Unit => GraphNode)(inputDefinition: Callable.InputDefinition): LinkedInputPort = {

    println(s"looking for ${inputDefinition.name} in \n")
    inputMapping.keys.foreach(println)

    val (outputPort, graphNode): (OutputPort, Option[GraphInputNode]) = inputDefinition match {
      case input if inputMapping.contains(input.name) => (inputMapping(input.name), None)
      case RequiredInputDefinition(n, womType) =>
        val result = RequiredGraphInputNode(s"$inputPrefix$n", womType)
        (result.singleOutputPort, Option(result)) // Read: Some(result)
      case OptionalInputDefinition(n, womType) =>
        val result = OptionalGraphInputNode(s"$inputPrefix$n", womType)
        (result.singleOutputPort, Option(result)) // Read: Some(result)
      case OptionalInputDefinitionWithDefault(n, womType, default) =>
        val result = OptionalGraphInputNodeWithDefault(s"$inputPrefix$n", womType, default)
        (result.singleOutputPort, Option(result)) // Read: Some(result)
    }

    LinkedInputPort(ConnectedInputPort(inputDefinition.name, inputDefinition.womType, outputPort)(callNodeRef), graphNode)
  }

  private[wom] implicit class EnhancedGraphNodeSet(val nodes: Set[GraphNode]) extends AnyVal {
    /**
      * Interpret this graph's "GraphInputNode"s as "Callable.InputDefinition"s
      */
    def inputDefinitions: Set[_ <: Callable.InputDefinition] = nodes collect {
      // TODO: FIXME: They might not be required!!
      case gin: GraphInputNode => RequiredInputDefinition(gin.name, gin.womType)
    }
  }
}
