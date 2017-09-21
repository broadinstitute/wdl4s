package wdl4s.wom.graph

import cats.syntax.traverse._
import cats.instances.list._
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.wdl.values.{WdlGlobFile, WdlValue}
import wdl4s.wom.callable.Callable.OutputDefinition
import wdl4s.wom.callable.{Callable, TaskDefinition, WorkflowDefinition}
import wdl4s.wom.graph.GraphNode.{GeneratedNodeAndNewInputs, LinkedInputPort}
import wdl4s.wom.graph.GraphNodePort.{GraphNodeOutputPort, OutputPort}

sealed abstract class CallNode extends GraphNode {
  def callable: Callable
  def callType: String

  def portBasedInputs: Set[GraphNodePort.InputPort]
  def expressionBasedInputs: Map[String, InstantiatedExpression]

  override final val inputPorts = portBasedInputs ++ expressionBasedInputs.values.flatMap(_.inputPorts)
}

final case class TaskCallNode private(override val name: String,
                                      callable: TaskDefinition,
                                      portBasedInputs: Set[GraphNodePort.InputPort],
                                      expressionBasedInputs: Map[String, InstantiatedExpression],
                                      globFiles: Map[String, WdlValue] => Set[WdlGlobFile]
                                     ) extends CallNode {
  val callType: String = "task"
  override val outputPorts: Set[GraphNodePort.OutputPort] = {
    callable.outputs.map(o => GraphNodeOutputPort(o.name, o.womType, this))
  }
}

final case class WorkflowCallNode private(override val name: String, callable: WorkflowDefinition, portBasedInputs: Set[GraphNodePort.InputPort], expressionBasedInputs: Map[String, InstantiatedExpression]) extends CallNode {
  val callType: String = "workflow"
  override val outputPorts: Set[GraphNodePort.OutputPort] = {
    callable.innerGraph.nodes.collect { case gon: GraphOutputNode => GraphNodeOutputPort(gon.name, gon.womType, this) }
  }
}

object TaskCall {
  def graphFromDefinition(taskDefinition: TaskDefinition): ErrorOr[Graph] = {

    def linkOutput(call: GraphNode)(output: OutputDefinition): ErrorOr[GraphNode] = call.outputByName(output.name).map(out => PortBasedGraphOutputNode(output.name, output.womType, out))

    import lenthall.validation.ErrorOr.ShortCircuitingFlatMap

    for {
      callWithInputs <- taskDefinition.callWithInputs(taskDefinition.name, Map.empty, Set.empty)
      outputs <- taskDefinition.outputs.toList.traverse(linkOutput(callWithInputs.node) _)
      callSet = Set[GraphNode](callWithInputs.node)
      inputsSet = callWithInputs.newInputs.toSet[GraphNode]
      outputsSet = outputs.toSet[GraphNode]
      graph <- Graph.validateAndConstruct(callSet ++ inputsSet ++ outputsSet)
    } yield graph
  }
}

object CallNode {

  final case class CallNodeAndNewInputs(node: CallNode, newInputs: Set[GraphInputNode]) extends GeneratedNodeAndNewInputs {
    def nodes: Set[GraphNode] = Set(node) ++ newInputs
  }


  /**
    * Create a CallNode for a Callable (task or workflow).
    *
    * If an input is supplied as a port from another Node, it gets wired in directly.
    * If an input is supplied as an expression, we try to create an InstantiatedExpression and include that in the call.
    * If an input is not supplied, it gets created as a GraphInputNode.
    *
    */
  def callWithInputs(name: String,
                     callable: Callable,
                     globFilesOption: Option[Map[String, WdlValue] => Set[WdlGlobFile]],
                     portInputs: Map[String, OutputPort],
                     expressionInputs: Set[GraphNodeInputExpression],
                     prefixSeparator: String = "."): ErrorOr[CallNodeAndNewInputs] = {

    val graphNodeSetter = new GraphNode.GraphNodeSetter()

    val instantiatedExpressionInputsAttempt: ErrorOr[Map[String, InstantiatedExpression]] = expressionInputs.toList traverse { _.instantiateExpressionWithInputName(graphNodeSetter) } map { _.toMap }

    instantiatedExpressionInputsAttempt map { instantiatedExpressionInputs =>
      val inputPortLinker = GraphNode.linkInputPort(name + prefixSeparator, portInputs, graphNodeSetter.get) _

      // Filter out the inputs we already have from expressions:
      val asYetUnsuppliedInputs = callable.inputs.filterNot(inputDef => instantiatedExpressionInputs.contains(inputDef.name))
      val linkedInputPortsAndGraphInputNodes = asYetUnsuppliedInputs map inputPortLinker

      val linkedInputPorts = linkedInputPortsAndGraphInputNodes.map(_.newInputPort)
      val graphInputNodes = linkedInputPortsAndGraphInputNodes collect { case LinkedInputPort(_, Some(gin)) => gin }

      val callNode = (callable, globFilesOption) match {
        case (taskDefinition: TaskDefinition, Some(globFiles)) =>
          TaskCallNode(name, taskDefinition, linkedInputPorts.toSet, instantiatedExpressionInputs, globFiles)
        case (workflowDefinition: WorkflowDefinition, None) =>
          WorkflowCallNode(name, workflowDefinition, linkedInputPorts.toSet, instantiatedExpressionInputs)
        case _ => throw new RuntimeException("this error situation of an invalid callable is hardcoded to never happen")
      }

      graphNodeSetter._graphNode = callNode
      CallNodeAndNewInputs(callNode, graphInputNodes.toSet)
    }
  }
}
