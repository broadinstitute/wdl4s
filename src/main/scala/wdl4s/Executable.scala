package wdl4s

/**
  * e.g. Call, Scatter, Workflow
  */
trait Executable {
  def upstream: Seq[Scope]
  def downstream: Seq[Scope]
  def workflow: Workflow
}
