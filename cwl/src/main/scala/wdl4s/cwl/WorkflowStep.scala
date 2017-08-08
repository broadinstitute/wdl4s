package wdl4s.cwl

import shapeless.{:+:, CNil}
import wdl4s.cwl.ScatterMethod._
import wdl4s.cwl.WorkflowStep.{Outputs, Run}
import wdl4s.wdl.command.CommandPart
import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wom.callable.Callable.RequiredInputDefinition
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.Expression
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

  def typedOutputs(cwlMap: Map[String, CwlFile]): WdlTypeMap = run.fold(RunOutputsToTypeMap).apply(cwlMap)

  def womGraphInputNodes: Set[GraphNode] = ???

  def womCallNode: GraphNode = ???

  def taskDefinitionInputs(typeMap: WdlTypeMap): Set[_ <: Callable.InputDefinition] =
    in.map { workflowStepInput =>

      val _source: String = workflowStepInput.source.flatMap(_.select[String]).get

      val mungedTypeMap = typeMap map {
        case (i, t) => RunOutputsToTypeMap.mungeId(i) -> t
      }

      val value = WorkflowStep.mungeInputId(_source)

      val tpe = mungedTypeMap(value)
      RequiredInputDefinition(_source, tpe)
    }.toSet


  def taskDefinitionOutputs(cwlMap: Map[String, CwlFile]): Set[Callable.OutputDefinition] = {

    val runnableIdToTypeMap: WdlTypeMap = run.fold(RunOutputsToTypeMap).apply(cwlMap)

    out.fold(WorkflowOutputsToOutputDefinition).apply(runnableIdToTypeMap)
  }

  def taskDefinition(typeMap: WdlTypeMap, cwlMap: Map[String, CwlFile]): TaskDefinition = {

    val id = this.id

    val commandTemplate: Seq[CommandPart] =
    //TODO: turn this select into a fold that supports other types of runnables
      run.select[CommandLineTool].map {
        clt =>
          clt.baseCommand.map(_.fold(BaseCommandToCommandParts)).toSeq.flatten ++
            clt.arguments.map(_.map(_.fold(ArgumentToCommandPart)).toSeq).toSeq.flatten
      }.toSeq.flatten

    val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression])

    val meta: Map[String, String] = Map.empty
    val parameterMeta: Map[String, String] = Map.empty

    val declarations: List[(String, Expression)] = List.empty

    TaskDefinition(
      id,
      commandTemplate,
      runtimeAttributes,
      meta,
      parameterMeta,
      taskDefinitionOutputs(cwlMap),
      taskDefinitionInputs(typeMap),
      declarations
    )
  }

  /**
    * In order to produce a map to lookup workflow
    * @param typeMap
    * @param cwlMap
    * @param workflow
    * @return
    */
  def callWithInputs(typeMap: WdlTypeMap,
                     cwlMap: Map[String, CwlFile],
                     workflow: Workflow,
                     knownNodes: Set[GraphNode],
                     workflowInputs: Map[String,
                       GraphNodeOutputPort]): Set[GraphNode] = {

    //TODO: Check to see if this step is already present in the graphNodes

    val haveWeSeenThisStep = knownNodes.flatMap{_ match {
      case TaskCallNode(name, _, _) => Set(name)
      case _ => Set.empty[String]
    }}.contains(id)

    if (haveWeSeenThisStep)
      Set.empty
    else {
      //need the outputs mapped from other workflow steps in order to pass in this map
      val workflowOutputsMap: (Map[String, OutputPort], Set[GraphNode]) = in.foldLeft((Map.empty[String, OutputPort], knownNodes)) {
        case ((map, upstreamNodes), workflowStepInput) =>

          val inputSource = workflowStepInput.source.flatMap(_.select[String]).get

          FullyQualifiedName(inputSource) match {
            case WorkflowStepOutputIdReference(_, _, stepId) =>

              //generate a node for this output

              def findThisInputInSet(set: Set[GraphNode]) ={

                val lookupSet: Set[OutputPort] =
                  for {
                    node <- set
                    outputPort <- node.outputPorts
                  } yield outputPort

                lookupSet.find(_.name == inputSource)
              }

              val foundInSeenNodes: Option[OutputPort] = findThisInputInSet(knownNodes)

              def lookupUpstreamNodes:Option[(Set[GraphNode], OutputPort)] = {
                //TODO: Option get!
                val step  =
                  workflow.steps.find { step =>
                    val lookup = WorkflowStepId(step.id).stepId
                    lookup == stepId
                  }.get

                val upstreamNodesForFoundStep:Set[GraphNode] = step.callWithInputs(typeMap, cwlMap, workflow, knownNodes,workflowInputs)

                findThisInputInSet(upstreamNodesForFoundStep).map(upstreamNodesForFoundStep -> _)
              }

              val newNodesAndOutputPort: Option[(Set[GraphNode],OutputPort)] = foundInSeenNodes.map(Set.empty[GraphNode] -> _) orElse lookupUpstreamNodes


              val (newNodes, outputPort) = newNodesAndOutputPort.get

              //TODO: Option . get makes kittens sad
              (map + (inputSource -> outputPort), upstreamNodes ++ newNodes)

            case _ => (map, upstreamNodes)
          }
      }

      val td = taskDefinition(typeMap, cwlMap)

      knownNodes ++ CallNode.callWithInputs(id, td, workflowInputs ++ workflowOutputsMap._1).nodes
    }
  }
}

/**
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStepOutput">WorkflowstepOutput</a>
  */
case class WorkflowStepOutput(id: String)

object WorkflowStep {

  def mungeInputId(in: String): String = {
    val afterHash = in.substring(in.indexOf("#") + 1)

    // Everything after the '/' if the '/' is present, otherwise the whole string.
    afterHash.substring(afterHash.lastIndexOf("/") + 1)
  }

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
