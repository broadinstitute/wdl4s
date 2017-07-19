package wdl4s.cwl

import shapeless.{:+:, CNil, Coproduct}
import ScatterMethod._
import wdl4s.cwl.WorkflowStep.{Inputs, Outputs, Run}

case class WorkflowStep(
                         id: Option[String], //not actually optional but can be declared as a key for this whole object for convenience
                         in: Inputs = Coproduct[Inputs](Array.empty[WorkflowStepInput]),
                         out: Outputs,
                         run: Run,
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
