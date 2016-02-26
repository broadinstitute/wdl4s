package wdl4s

trait WorkflowScoped extends Scope {
  def workflow: Workflow = ancestry.collect({ case w: Workflow => w }).headOption.getOrElse(
    throw new IllegalStateException(s"Grammar constraint violation: $fullyQualifiedName should be contained in a workflow")
  )
}
