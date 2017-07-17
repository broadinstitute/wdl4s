package wdl4s.cwl

import shapeless.{:+:, CNil}
import ScatterMethod._
import wdl4s.cwl.WorkflowStep.WorkflowStepRun

case class WorkflowStep(
  id: Option[String], //not actually optional but can be declared as a key for this whole object for convenience
  in: WorkflowStepInputs,
  out: WorkflowStepOutputs,
  run: WorkflowStepRun,
  requirements: Option[Array[Requirement]] = None,
  hints: Option[Array[String]] = None, //TODO: should be 'Any' type
  label: Option[String] = None,
  doc: Option[String] = None,
  scatter: Option[String :+: Array[String] :+: CNil] = None,
  scatterMethod: Option[ScatterMethod] = None)

/**
  * @see <a href="http://www.commonwl.org/v1.0/Workflow.html#WorkflowStepOutput">WorkflowstepOutput</a>
  *
  * @param id
  */
case class WorkflowStepOutput(id: String)

object WorkflowStep {
  type WorkflowStepRun =
    String :+:
      CommandLineTool :+:
      ExpressionTool :+:
      Workflow :+:
      CNil

}
