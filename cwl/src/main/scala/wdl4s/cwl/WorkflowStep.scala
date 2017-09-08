package wdl4s.cwl

import shapeless._
import wdl4s.cwl.ScatterMethod._
import wdl4s.cwl.WorkflowStep.{Outputs, Run}
import wdl4s.wom.callable.Callable.RequiredInputDefinition
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.graph.GraphNodePort.{GraphNodeOutputPort, OutputPort}
import wdl4s.wom.graph.{CallNode, GraphNode, TaskCallNode}

/**
  * An individual job to run.
  *
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStep">CWL Spec | Workflow Step</a>
  * @param run Purposefully not defaulted as it's required and it is unreasonable to not have something to run.
  */
case class WorkflowStep(
                         id: String,
                         in: Array[WorkflowStepInput] = Array.empty,
                         out: Outputs,
                         run: Run,
                         requirements: Option[Array[Requirement]] = None,
                         hints: Option[Array[CwlAny]] = None,
                         label: Option[String] = None,
                         doc: Option[String] = None,
                         scatter: Option[String :+: Array[String] :+: CNil] = None,
                         scatterMethod: Option[ScatterMethod] = None) {

  def typedOutputs: WdlTypeMap = run.fold(RunOutputsToTypeMap)

  def fileName: Option[String] = run.select[String]

  /**
    * In order to produce a map to lookup workflow
    * @param typeMap
    * @param workflow
    * @return
    */

  def callWithInputs(typeMap: WdlTypeMap,
                     workflow: Workflow,
                     knownNodes: Set[GraphNode],
                     workflowInputs: Map[String, GraphNodeOutputPort]): Set[GraphNode] = {

    def taskDefinition: TaskDefinition = {


      val taskDefinitionInputs: Set[_ <: Callable.InputDefinition] =
        in.map { workflowStepInput =>

          val _source: String = workflowStepInput.source.flatMap(_.select[String]).get

          val mungedTypeMap = typeMap map {
            case (i, t) => RunOutputsToTypeMap.mungeId(i) -> t
          }

          val value = FullyQualifiedName(_source) match {
            case WorkflowStepOutputIdReference(_, stepOutputId, _) => stepOutputId
            case WorkflowInputId(_, inputId) => inputId
          }

          val tpe = mungedTypeMap(value)
          RequiredInputDefinition(_source, tpe)
        }.toSet


      val taskDefinitionOutputs: Set[Callable.OutputDefinition] = {

        //this map will only match on the "id" field of the fully qualified name
        val idMap = typedOutputs map {
          case (id, tpe) => RunOutputId(id).outputId -> tpe
        }

        out.fold(WorkflowOutputsToOutputDefinition).apply(idMap)
      }


      val id = this.id

      //This task has inputs and outputs
      val cltTask = run.select[CommandLineTool].map(_.taskDefinition).get



      ???

    }

    def foldInputs(mapAndNodes: (Map[String, OutputPort], Set[GraphNode]), workflowStepInput: WorkflowStepInput): (Map[String, OutputPort], Set[GraphNode]) =
      mapAndNodes match {
        case ((map, knownNodes)) => //shadowing knownNodes on purpose to avoid accidentally referencing the outer one

          val inputSource = workflowStepInput.source.flatMap(_.select[String]).get

          def findThisInputInSet(set: Set[GraphNode]) ={

            val lookupSet: Set[OutputPort] =
              for {
                node <- set
                outputPort <- node.outputPorts
              } yield outputPort

            lookupSet.find(_.name == inputSource)
          }

          def lookupUpstreamNodes(nodeId: String):Option[(Set[GraphNode], OutputPort)] = {
            //TODO: Option get!
            val step  =
              workflow.steps.find { step =>
                val lookup = WorkflowStepId(step.id).stepId
                lookup == nodeId
              }.get

            val upstreamNodesForFoundStep:Set[GraphNode] = step.callWithInputs(typeMap, workflow, knownNodes, workflowInputs)

            findThisInputInSet(upstreamNodesForFoundStep).map(upstreamNodesForFoundStep -> _)
          }

          FullyQualifiedName(inputSource) match {
            case _: WorkflowInputId =>

              val foundInSeenNodes = findThisInputInSet(knownNodes)

              val newNodesAndOutputPort: Option[OutputPort] =
                foundInSeenNodes

              val outputPort = newNodesAndOutputPort.get

              (map + (inputSource -> outputPort), knownNodes)
            case WorkflowStepOutputIdReference(_, _, stepId) =>

              val foundInSeenNodes: Option[OutputPort] = findThisInputInSet(knownNodes)

              val newNodesAndOutputPort: Option[(Set[GraphNode],OutputPort)] = foundInSeenNodes.map(Set.empty[GraphNode] -> _) orElse lookupUpstreamNodes(stepId)

              //TODO: Option . get makes kittens sad
              val (newNodes, outputPort) = newNodesAndOutputPort.get

              (map + (inputSource -> outputPort), knownNodes ++ newNodes)
            case _ => (map, knownNodes) //TODO: This should "fail fast" as Invalid here as it won't pass validation
          }
      }

    val haveWeSeenThisStep = knownNodes.flatMap{_ match {
      case TaskCallNode(name, _, _, _) => Set(name)
      case _ => Set.empty[String]
    }}.contains(id)

    if (haveWeSeenThisStep)
      knownNodes
    else {
      val workflowOutputsMap: (Map[String, OutputPort], Set[GraphNode]) =
        in.foldLeft((Map.empty[String, OutputPort], knownNodes)) (foldInputs)

      val td = taskDefinition

      // TODO: Fixme!
      CallNode.callWithInputs(id, td, workflowInputs ++ workflowOutputsMap._1, Set.empty).getOrElse(???).nodes ++ workflowOutputsMap._2
    }
  }
}

/**
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStepOutput">WorkflowstepOutput</a>
  */
case class WorkflowStepOutput(id: String)

object WorkflowStep {

  val emptyOutputs: Outputs = Coproduct[Outputs](Array.empty[String])

  type Run =
    String :+:
      CommandLineTool :+:
      ExpressionTool :+:
      Workflow :+:
      CNil

  type Outputs =
    Array[String] :+:
      Array[WorkflowStepOutput] :+:
      CNil
}
