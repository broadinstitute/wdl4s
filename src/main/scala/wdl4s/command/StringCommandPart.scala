package wdl4s.command

import wdl4s.expression.WdlFunctions
import wdl4s.values.WdlValue
import wdl4s._

case class StringCommandPart(literal: String) extends CommandPart {
  override def toString: String = literal

  override def instantiate(task: Task,
                           parameters: Map[String, WdlValue],
                           functions: WdlFunctions[WdlValue],
                           valueMapper: WdlValue => WdlValue): String = literal

  override def instantiate(call: Call,
                           parameters: WorkflowCoercedInputs,
                           functions: WdlFunctions[WdlValue],
                           shards: Map[Scatter, Int] = Map.empty[Scatter, Int],
                           valueMapper: WdlValue => WdlValue): String = literal
}
