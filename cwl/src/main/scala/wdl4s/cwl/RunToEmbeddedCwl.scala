package wdl4s.cwl

import shapeless._

object RunToEmbeddedCwl extends Poly1 {
  implicit def commandLineTool =
    at[CommandLineTool] {
      clt =>
        (_: Map[String, Cwl]) =>
         Coproduct[WorkflowStep.Run](clt)
    }

  implicit def string = at[String] {
    fileName =>
      (cwlMap: Map[String, Cwl]) =>
        cwlMap(fileName).fold(CwlToRun)
  }


  implicit def expressionTool = at[ExpressionTool] {
    et =>
      (_: Map[String, Cwl]) =>
        Coproduct[Run](et)
  }

  implicit def workflow = at[Workflow] {
    wf =>
      (_: Map[String, Cwl]) =>
        Coproduct[WorkflowStep.Run](wf)
  }
}

