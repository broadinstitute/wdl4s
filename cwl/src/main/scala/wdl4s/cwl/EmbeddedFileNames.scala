
object EmbeddedFileNames extends Poly1 {
  val workflowStepLens = lens[Workflow].steps
  val workflowStepRunLens = lens[WorkflowStep].run

  implicit def string =
    at[String] {
      List(_)
    }

  implicit def workflow =
    at[Workflow] {
      _ => List.empty[String]

    }

  implicit def commandLineTool =
    at[CommandLineTool] {
      clt =>
          List.empty[String]
    }
  implicit def workflow =
    at[ExpressionTool] {
      _ => List.empty[String]

    }
}
