package wdl4s.cwl

import shapeless._

object AddEmbeddedCwl extends Poly1 {
  val workflowStepLens = lens[Workflow].steps
  val workflowStepRunLens = lens[WorkflowStep].run

  implicit def workflow =
    at[Workflow] {
      wf =>
        (map: Map[String, Cwl]) =>
          val wf_ =
            workflowStepLens.modify(wf){
              _.map{ step =>
                workflowStepRunLens.modify(step)(_.fold(RunToEmbeddedCwl).apply(map))
              }
            }
          Coproduct[Cwl](wf_)
    }

  implicit def commandLineTool =
    at[CommandLineTool] {
      clt =>
        (_: Map[String, Cwl]) =>
          Coproduct[Cwl](clt)
    }
}
