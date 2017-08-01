package wdl4s.cwl

import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wdl.command.CommandPart
import wdl4s.wom.callable.Callable.{InputDefinition, RequiredInputDefinition}
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.Expression
import wdl4s.wom.graph.{CallNode, GraphNode, RequiredGraphInputNode}
import shapeless.{:+:, CNil}
import ScatterMethod._
import wdl4s.cwl.CwlType.CwlType
import wdl4s.cwl.WorkflowStep.{Outputs, Run}
import wdl4s.wdl.types.WdlType
import wdl4s.wom.graph.CallNode.CallWithInputs
import wdl4s.wom.graph.GraphNodePort.GraphNodeOutputPort

/**
  * An individual job to run.
  *
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStep">CWL Spec | Workflow Step</a>
  *
  * @param id
  * @param in
  * @param out
  * @param run Purposefully not defaulted as it's required and it is unreasonable to not have something to run.
  * @param requirements
  * @param hints
  * @param label
  * @param doc
  * @param scatter
  * @param scatterMethod
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

  def typedOutputs(cwlMap: Map[String, CwlFile]): WdlTypeMap = run.fold(RunToTypeMap).apply(cwlMap)

  def womGraphInputNodes: Set[GraphNode] = ???

  def womCallNode: GraphNode = ???

  def taskDefinitionInputs(typeMap: WdlTypeMap):  Set[_ <: Callable.InputDefinition] =
    in.map{workflowStepInput =>

      val _source: String = workflowStepInput.source.flatMap(_.select[String]).get

      val mungedTypeMap = typeMap map {
        case (id, tpe) => RunToTypeMap.mungeId(id) -> tpe
      }

      val value = WorkflowStep.mungeInputId(_source)

      val tpe = mungedTypeMap(value)
      RequiredInputDefinition(_source, tpe)
    }.toSet


  def taskDefinitionOutputs(cwlMap: Map[String, CwlFile]): Set[Callable.OutputDefinition] = {

    val runnableFQNTypeMap: WdlTypeMap = run.fold(RunToTypeMap).apply(cwlMap)

    val runnableIdToTypeMap = runnableFQNTypeMap.map {
      case (id, tpe) => RunToTypeMap.mungeId(id) -> tpe
    }

    out.fold(WorkflowOutputsToOutputDefinition).apply(runnableIdToTypeMap)
  }

  def taskDefinition(typeMap: WdlTypeMap, cwlMap: Map[String, CwlFile]): TaskDefinition = {

    val id = this.id

    val commandTemplate: Seq[CommandPart] = run.select[CommandLineTool].map(_.baseCommand.get.fold(BaseCommandToCommandParts)).toSeq.flatten

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

  def callWithInputs(typeMap: WdlTypeMap, cwlMap: Map[String, CwlFile], workflow: Workflow): CallWithInputs = {

    val workflowInputs =
      workflow.inputs.map {
          workflowInput =>
            val tpe = workflowInput.`type`.flatMap(_.select[CwlType]).map(cwlTypeToWdlType).get
            val node = RequiredGraphInputNode(workflowInput.id, tpe)
            workflowInput.id -> GraphNodeOutputPort(workflowInput.id, tpe, node)
        }.toMap



    //need the outputs mapped from other workflow steps in order to pass in this map
    val workflowOutputsMap = in.flatMap{
      workflowStepInput =>

        def lookupStepWithOutput(stepId: String, outputId: String): (WorkflowStep, (String, WdlType)) = {
          val step = workflow.steps.find{step =>
            val lookup = WorkflowStepId(step.id).stepId
            lookup == stepId
          }.get

          step.typedOutputs(cwlMap).map(step -> _)
            .find{
              case (_, (stepOutputId, _)) =>
                WorkflowStepOutputId(stepOutputId).outputId == outputId
            }.get
        }

        val inputSource = workflowStepInput.source.flatMap(_.select[String]).get

        FullyQualifiedName(inputSource) match {
          case WorkflowStepOutputIdReference(_, stepOutputId, stepId) =>
            val (step, (id, tpe)) = lookupStepWithOutput(stepId, stepOutputId)
            List((inputSource -> GraphNodeOutputPort(inputSource, tpe, step.callWithInputs(typeMap, cwlMap, workflow).call)))
          case _ => List.empty[(String, GraphNodeOutputPort)]
        }

    }.toMap

    val td = taskDefinition(typeMap, cwlMap)

    CallNode.callWithInputs(id, td, workflowInputs ++ workflowOutputsMap)
  }



  def womDefinition: TaskDefinition =  {
    val commandTemplate : Seq[CommandPart] = WorkflowStep.runToCommandTemplate(run)
    val runtimeAttributes: RuntimeAttributes = ??? //requirements.

    val meta: Map[String, String] = ???
    val parameterMeta: Map[String, String] = ???
    val outputs: Set[Callable.OutputDefinition] = ???
    val inputs: Set[_ <: Callable.InputDefinition] = ???
    val declarations: List[(String, Expression)] = ???

    TaskDefinition(
      id,
      commandTemplate,
      runtimeAttributes,
      meta,
      parameterMeta,
      outputs,
      inputs,
      declarations
    )
  }
}

/**
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStepOutput">WorkflowstepOutput</a>
  *
  * @param id
  */
case class WorkflowStepOutput(id: String)

object WorkflowStep {

  def mungeInputId(in: String): String = {
    val afterHash = in.substring(in.indexOf("#") + 1, in.length)

    if (afterHash.contains("/"))
      afterHash.substring(afterHash.lastIndexOf("/") + 1, afterHash.length)
    else
      afterHash
  }


  def runToCommandTemplate: Run => Seq[CommandPart] = ???

  def fromInputs: Inputs => InputDefinition = ???

  def fromResourceRequirement: ResourceRequirement => RuntimeAttributes = ???

  type Run =
    String :+:
      CommandLineTool :+:
      ExpressionTool :+:
      Workflow :+:
      CNil

  type Inputs =
    Array[WorkflowStepInput] :+:
      Map[WorkflowStepInputId, WorkflowStepInputSource] :+:
      Map[WorkflowStepInputId, WorkflowStepInput] :+:
      CNil

  type Outputs =
    Array[String] :+:
      Array[WorkflowStepOutput] :+:
      CNil

}
