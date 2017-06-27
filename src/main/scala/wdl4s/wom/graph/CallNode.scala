package wdl4s.wom.graph

import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.wom.callable.Callable.{OptionalInputDefinition, OptionalInputDefinitionWithDefault, OutputDefinition, RequiredInputDefinition}
import wdl4s.wom.callable.{Callable, TaskDefinition, WorkflowDefinition}
import wdl4s.wom.graph.GraphNodePort.{ConnectedInputPort, DeclarationOutputPort, InputPort, OutputPort}

import scala.language.postfixOps
import cats.implicits._

final case class CallNode private(name: String, callable: Callable, inputPorts: Set[GraphNodePort.InputPort]) extends GraphNode {
  val callType: String = callable match {
    case _: TaskDefinition => "task"
    case _: WorkflowDefinition => "workflow"
  }
  override val outputPorts: Set[GraphNodePort.OutputPort] = callable match {
    case t: TaskDefinition => t.outputs.map(o => DeclarationOutputPort(o.name, o.womType, this))
    case w: WorkflowDefinition => w.innerGraph.nodes.collect { case gon: GraphOutputNode => DeclarationOutputPort(gon.name, gon.womType, this) }
  }
}

object TaskCall {
  def graphFromDefinition(taskDefinition: TaskDefinition): ErrorOr[Graph] = {

    def linkOutput(call: GraphNode)(output: OutputDefinition): ErrorOr[GraphNode] = call.outputByName(output.name).map(out => GraphOutputNode(output.name, output.womType, out))

    val (call, inputs) = CallNode.callWithInputs(taskDefinition.name, taskDefinition, Map.empty)
    val outputsValidation = taskDefinition.outputs.toList.traverse(linkOutput(call) _)

    import lenthall.validation.ErrorOr.ShortCircuitingFlatMap
    outputsValidation flatMap { outputs => Graph.validateAndConstruct(Set[GraphNode](call).union(inputs).union(outputs.toSet)) }
  }
}

object CallNode {

  private class GraphNodeSetter {
    var _graphNode: GraphNode = _
    private def getGraphNode = _graphNode
    def get: () => GraphNode = () => getGraphNode
  }

  /**
    * Create a CallNode for a Callable (task or workflow).
    *
    * If an input is supplied, it gets wired in as appropriate.
    * If an input is not supplied, it gets created as a GraphInputNode.
    *
    * The returned value is a tuple of (
    *   _1: the CallNode
    *   _2: any GraphInputNodes we created for unsupplied inputs
    * )
    */
  def callWithInputs(name: String, callable: Callable, inputMapping: Map[String, OutputPort]): (CallNode, Set[GraphNode]) = {

    def linkInputPort(inputDefinition: Callable.InputDefinition, callNodeRef: () => GraphNode): (InputPort, Option[GraphNode]) = {
      val (outputPort, graphNode): (OutputPort, Option[GraphNode]) = inputDefinition match {
        case input if inputMapping.contains(input.name) => (inputMapping(input.name), None)
        case RequiredInputDefinition(n, womType) =>
          val result = RequiredGraphInputNode(s"${callable.name}.$n", womType)
          (result.singleOutputPort, Some(result))
        case OptionalInputDefinition(n, womType) =>
          val result = OptionalGraphInputNode(s"${callable.name}.$n", womType)
          (result.singleOutputPort, Some(result))
        case OptionalInputDefinitionWithDefault(n, womType, default) =>
          val result = OptionalGraphInputNodeWithDefault(s"${callable.name}.$n", womType, default)
          (result.singleOutputPort, Some(result))
      }

      (ConnectedInputPort(inputDefinition.name, inputDefinition.womType, outputPort, callNodeRef), graphNode)
    }

    val graphNodeSetter = new GraphNodeSetter()
    val linkedInputPortsAndGraphInputNodes: Set[(InputPort, Option[GraphNode])] = callable.inputs.map(linkInputPort(_, graphNodeSetter.get))

    val linkedInputPorts = linkedInputPortsAndGraphInputNodes.map(_._1)
    val graphInputNodes = linkedInputPortsAndGraphInputNodes collect { case (_, Some(gin)) => gin }

    val callNode = CallNode(name, callable, linkedInputPorts)

    graphNodeSetter._graphNode = callNode
    (callNode, graphInputNodes)
  }
}
