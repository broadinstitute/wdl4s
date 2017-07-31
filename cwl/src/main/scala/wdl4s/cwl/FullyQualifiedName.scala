package wdl4s.cwl

trait FullyQualifiedName {
  val fileName: String
}

case class WorkflowInputId private (fileName: String, inputId: String) extends FullyQualifiedName

object WorkflowInputId {
  def apply(in: String): WorkflowInputId = {
    val Array(fileName, id) = in.split("#")

    WorkflowInputId(fileName, id)
  }
}

case class WorkflowStepId private (fileName: String, stepId: String) extends FullyQualifiedName

object WorkflowStepId {
  def apply(in: String): WorkflowStepId = {
    val Array(fileName, id) = in.split("#")

    WorkflowStepId(fileName, id)
  }
}

case class WorkflowStepOutputId private (fileName: String, outputId: String) extends FullyQualifiedName

object WorkflowStepOutputId {
  def apply(in: String): WorkflowStepOutputId = {
    val Array(fileName, id) = in.split("#")

    WorkflowStepOutputId(fileName, id)
  }
}


case class WorkflowStepOutputIdReference(fileName: String, stepOutputId: String, stepId: String) extends FullyQualifiedName

object WorkflowStepOutputIdReference {
  def apply(in: String): WorkflowStepOutputIdReference = {
    val Array(fileName, stepAndid) = in.split("#")
    val Array(step, id) = stepAndid.split("/")

    WorkflowStepOutputIdReference(fileName, id, step)
  }
}

object FullyQualifiedName {
  def apply(in: String): FullyQualifiedName = {

   val Array(ignored, after) = in.split("#")

   if (after.contains("/"))
     WorkflowStepOutputIdReference(in)
    else
     WorkflowInputId(in)
  }
}


