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


case class WorkflowStepInputSource(fileName: String, stepOutputId: String, stepId: String) extends FullyQualifiedName

object WorkflowStepInputSource {
  def apply(in: String): WorkflowStepInputSource = {
    val Array(fileName, stepAndid) = in.split("#")
    val Array(step, id) = stepAndid.split("/")

    WorkflowStepInputSource(fileName, id, step)
  }
}

sealed trait RunOutputId {
  def fileName: String
  def outputId: String
}

case class SameFileRunOutputId(fileName: String, outputId: String, uuid: String, stepId: String) extends RunOutputId

case class DifferentFileRunOutputId(fileName: String, outputId: String) extends RunOutputId

object RunOutputId {
  def apply(in: String): RunOutputId = {
    val Array(fileName, stepAndid) = in.split("#")

    if (stepAndid.contains("/")) {
      val Array(step, uuid, id) = stepAndid.split("/")
      SameFileRunOutputId(fileName, id, uuid, step)
    } else
      DifferentFileRunOutputId(fileName, stepAndid)
  }
}
object FullyQualifiedName {
  def apply(in: String): FullyQualifiedName = {

   val Array(ignored, after) = in.split("#")

   if (after.contains("/"))
     WorkflowStepInputSource(in)
    else
     WorkflowInputId(in)
  }
}


