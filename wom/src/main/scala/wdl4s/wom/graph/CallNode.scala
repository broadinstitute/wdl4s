package wdl4s.wom.graph

import cats.instances.list._
import cats.syntax.traverse._
import lenthall.validation.ErrorOr.ErrorOr
import shapeless.{:+:, CNil, Coproduct}
import wdl4s.wom.callable.Callable.{DeclaredInputDefinition, GraphInputDefinition, InputDefinition, OutputDefinition}
import wdl4s.wom.callable.{Callable, TaskDefinition, WorkflowDefinition}
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.CallNode.InputDefinitionMappings
import wdl4s.wom.graph.GraphNode.{GeneratedNodeAndNewInputs, LinkedInputPort}
import wdl4s.wom.graph.GraphNodePort.{GraphNodeOutputPort, OutputPort}

sealed abstract class CallNode extends GraphNode {
  def callable: Callable
  def callType: String

  def inputDefinitionMappings: InputDefinitionMappings
}

final case class TaskCallNode private(override val name: String,
                                      callable: TaskDefinition,
                                      override val inputPorts: Set[GraphNodePort.InputPort],
                                      inputDefinitionMappings: InputDefinitionMappings) extends CallNode {
  val callType: String = "task"
  override val outputPorts: Set[GraphNodePort.OutputPort] = {
    callable.outputs.map(o => GraphNodeOutputPort(o.name, o.womType, this)).toSet
  }
}

final case class WorkflowCallNode private(override val name: String,
                                          callable: WorkflowDefinition,
                                          override val inputPorts: Set[GraphNodePort.InputPort],
                                          inputDefinitionMappings: InputDefinitionMappings) extends CallNode {
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
      callWithInputs <- CallNode.callWithInputs(taskDefinition.name, taskDefinition, Map.empty, Set.empty, prefixSeparator = taskDefinition.prefixSeparator)
      outputs <- taskDefinition.outputs.traverse(linkOutput(callWithInputs.node) _)
      callSet = Set[GraphNode](callWithInputs.node)
      inputsSet = callWithInputs.newInputs.toSet[GraphNode]
      outputsSet = outputs.toSet[GraphNode]
      graph <- Graph.validateAndConstruct(callSet ++ inputsSet ++ outputsSet)
    } yield graph
  }
}

object CallNode {
  object InputDefinitionFold {
    private [graph] def empty = new InputDefinitionFold(Map.empty, Set.empty)
  }
  private [graph] final case class InputDefinitionFold(mappings: InputDefinitionMappings, linkedInputPorts: Set[LinkedInputPort])

  type InputDefinitionPointer = OutputPort :+: InstantiatedExpression :+: WomExpression :+: CNil
  type InputDefinitionMappings = Map[InputDefinition, InputDefinitionPointer]

  final case class CallNodeAndNewInputs(node: CallNode, newInputs: Set[GraphInputNode]) extends GeneratedNodeAndNewInputs {
    def nodes: Set[GraphNode] = Set(node) ++ newInputs
  }

  /**
    * Don't use this directly; go via callWithInputs to make sure everything's in order when constructing a CallNode.
    */
  private[graph] def apply(name: String,
                           callable: Callable,
                           inputPorts: Set[GraphNodePort.InputPort],
                           inputDefinitionMappings: InputDefinitionMappings): CallNode = callable match {
    case t: TaskDefinition => TaskCallNode(name, t, inputPorts, inputDefinitionMappings)
    case w: WorkflowDefinition => WorkflowCallNode(name, w, inputPorts, inputDefinitionMappings)
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
                     portInputs: Map[String, OutputPort],
                     expressionInputs: Set[GraphNodeInputExpression],
                     prefixSeparator: String = "."): ErrorOr[CallNodeAndNewInputs] = {

    val graphNodeSetter = new GraphNode.GraphNodeSetter()

    val instantiatedExpressionInputsAttempt: ErrorOr[Map[String, InstantiatedExpression]] = expressionInputs.toList traverse { _.instantiateExpressionWithInputName(graphNodeSetter) } map { _.toMap }

    instantiatedExpressionInputsAttempt map { instantiatedExpressionInputs =>
      val inputPortLinker = GraphNode.linkInputPort(name + prefixSeparator, portInputs, graphNodeSetter.get) _

      // We didn't find an instantiated expression but there still might be a non external output port (remember we don't
      // want to to override DeclaredInputDefinition with external inputs). Otherwise use the wom expression.
      def forDeclaredInput(declared: DeclaredInputDefinition) = {
        portInputs.collectFirst({
          case (portName, outputPort) if
          portName == declared.name && !outputPort.graphNode.isInstanceOf[ExternalGraphInputNode] =>
            Coproduct[InputDefinitionPointer](outputPort)
        }) getOrElse Coproduct[InputDefinitionPointer](declared.expression)
      }

      /*
        * This is a bit misleading as it makes it seem like expressions have precedence over output ports. They don't.
        * What matters is that we look for an expression OR a pre-existing output port before we generate a 
        * new input node. The inputPortLinker is the one that checks for an existing outputPort before creating the 
        * input node if necessary.
       */
      def foldFunction(fold: InputDefinitionFold, inputDefinition: InputDefinition): InputDefinitionFold = {
        instantiatedExpressionInputs.get(inputDefinition.name) match {
          case Some(expression) =>
            fold.copy(mappings = fold.mappings + (inputDefinition -> Coproduct[InputDefinitionPointer](expression)))
          case None => inputDefinition match {
            case declared: DeclaredInputDefinition =>
              fold.copy(mappings = fold.mappings + (inputDefinition -> forDeclaredInput(declared)))
            case gid: GraphInputDefinition =>
              val linked = inputPortLinker(gid)
              fold.copy(
                mappings = fold.mappings + (inputDefinition -> Coproduct[InputDefinitionPointer](linked.newInputPort.upstream)),
                linkedInputPorts = fold.linkedInputPorts + linked
              )
          }
        }
      }

      val foldedInputs = callable.inputs.foldLeft(InputDefinitionFold.empty)(foldFunction)

      val graphInputNodes = foldedInputs.linkedInputPorts.flatMap(_.newGraphInput)
      val newInputPorts = foldedInputs.linkedInputPorts.map(_.newInputPort)
      val expressionInputPorts = instantiatedExpressionInputs.flatMap(_._2.inputPorts)
      val inputDefinitionMappings = foldedInputs.mappings

      val callNode = CallNode(name, callable, newInputPorts ++ expressionInputPorts, inputDefinitionMappings)


      graphNodeSetter._graphNode = callNode
      CallNodeAndNewInputs(callNode, graphInputNodes)
    }
  }
}
