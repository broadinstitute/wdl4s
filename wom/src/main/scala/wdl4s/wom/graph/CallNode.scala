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
import wdl4s.wom.graph.GraphNodePort.{ConnectedInputPort, GraphNodeOutputPort, OutputPort}

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
    * If an input is not supplied, it gets created as a GraphInputNode if the input is allowed to be overridden by graph inputs,
    * otherwise the declared value is used.
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

      /*
        * This method is used when we didn't find an instantiated expression for a DeclaredInputDefinition,
        * but there still might be a non external output port that matches 
        * (remember we don't want to to override DeclaredInputDefinition with external inputs).
        * In that case, create a linked input port for it so it can be exposed at the call level (the same way the inputPortLinker would),
        * and return this output port as the mapping for the input definition.
        * Otherwise simply use the wom expression.
       */
      def forDeclaredInput(declared: DeclaredInputDefinition) = {
        portInputs.collectFirst({
          case (portName, outputPort) if
          portName == declared.name && !outputPort.graphNode.isInstanceOf[ExternalGraphInputNode] =>
            val linkedInputPort = LinkedInputPort(ConnectedInputPort(declared.name, declared.womType, outputPort, graphNodeSetter.get), None)
            (Option(linkedInputPort), Coproduct[InputDefinitionPointer](outputPort))
        }) getOrElse {
          (None, Coproduct[InputDefinitionPointer](declared.expression))
        }
      }

      /*
        * This method is used to fold over input definitions and
        * 1) Find a mapping for each input definition of the callable
        * 2) Collect all LinkedInputPorts that get created in the process
        * 
        * An input definition can be mapped to (exclusively)
        * 1) An instantiated expression (from expressionInputs)
        * 2) An output port (from portInputs or from a newly created LinkedInputPort)
        * 3) A wom expression
        * 
        * The DeclaredInputDefinition diverts from the other InputDefinitions in terms of precedence rules, so we'll lay out
        * the rules in 2 parts.
        * 
        * @ DeclaredInputDefinition:
        * The specificity of DeclaredInputDefinition lies in the fact that it cannot be overridden by external graph inputs
        * (inputs coming from outside the final graph, which at this point can only be workflow inputs).
        * 
        * Therefore, here are the rules used to find the mapping for a DeclaredInputDefinition, applied in order:
        *   A) 
        *     a) There is a matching instantiated expression
        *       OR
        *     b) There is a matching output port (in portInputs) attached to a graph node that is NOT an ExternalGraphInputNode
        *   Use whichever mapping satisfies a) or b), if any. It is not expected that a) and b) match at the same time for the same input definition.
        *   In practice a) is checked before b) but this does not hold any meaning in terms of precedence and should not be relied upon. 
        *   
        *   B) Use the wom expression declared in the DeclaredInputDefinition
        * 
        * @ GraphInputDefinition (input definitions that can be overridden by graph inputs, namely
        * RequiredInputDefinition, OptionalInputDefinition, OptionalInputDefinitionWithDefault):
        * 
        *   C) 
        *     a) There is a matching instantiated expression
        *       OR
        *     b) There is a matching output port (in portInputs)
        *   Use whichever mapping satisfies a) or b), if any. It is not expected that a) and b) match at the same time for the same input definition.
        *   In practice a) is checked before b) but this does not hold any meaning in terms of precedence and should not be relied upon. 
        *
        *   D) Create a new graph input node (corresponding to the type of GraphInputDefinition: required, optional
        *   or optionalWithDefault. The output port from this newly created graph input node is used as a mapping for the input definition.
        * 
        * 
        * In A.b, C.b and D, a LinkedInputPort is created. This is because we've determined that the mapping to the input definition is an
        * output port, for which we need a corresponding input port that will become an input port of the call node. Indeed, the
        * call now needs on those input ports to be satisfied before it can be run. The input ports from the instantiated expressions
        * will also contribute to the set of input ports for the call.
        * 
        * Additionally, if rule D) is applied, ExternalGraphInputNode have also be created because we were unable
        * to satisfy a GraphInputDefinition. Those nodes are the "inputs" returned by callWithInputs.
       */
      def foldFunction(fold: InputDefinitionFold, inputDefinition: InputDefinition): InputDefinitionFold = {
        instantiatedExpressionInputs.get(inputDefinition.name) match {
          case Some(expression) =>
            fold.copy(mappings = fold.mappings + (inputDefinition -> Coproduct[InputDefinitionPointer](expression)))
          case None => inputDefinition match {
            case declared: DeclaredInputDefinition =>
              val (newLinkedInputPort, mapping) = forDeclaredInput(declared)
              fold.copy(
                mappings = fold.mappings + (inputDefinition -> mapping),
                linkedInputPorts = fold.linkedInputPorts ++ newLinkedInputPort
              )
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
