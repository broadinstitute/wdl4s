package wdl4s

trait WdlTest {
  def namespace: WdlNamespaceWithWorkflow
  def workflow = namespace.workflow
  def getTask(name: String): Task = namespace.tasks.find(_.unqualifiedName == name).get
  def getCall(name: String): Call = namespace.workflow.calls.find(_.unqualifiedName == name).get
  def getScatter(index: Int): Scatter = namespace.resolve(namespace.workflow.unqualifiedName + ".$scatter_" + index).get.asInstanceOf[Scatter]
  def getIf(index: Int): If = namespace.resolve(namespace.workflow.unqualifiedName + ".$if_" + index).get.asInstanceOf[If]
}
