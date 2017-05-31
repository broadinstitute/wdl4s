package wdl4s.wom.callable

import wdl4s.wdl._
import scala.language.postfixOps

final case class WorkflowDefinition(name: String,
                                    inputs: Set[_ >: Callable.InputDefinition],
                                    outputs: Set[Callable.OutputDefinition],
                                    graph: Set[_ >: GraphNode],
                                    meta: Map[String, String],
                                    parameterMeta: Map[String, String]) extends Callable {

  override def toString = s"[Workflow $name]"
}
