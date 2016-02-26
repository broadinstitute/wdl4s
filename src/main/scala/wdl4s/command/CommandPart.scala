package wdl4s.command

import wdl4s.expression.WdlFunctions
import wdl4s.values.WdlValue
import wdl4s._

trait CommandPart {
  def instantiate(task: Task,
                  parameters: CallInputs,
                  functions: WdlFunctions[WdlValue],
                  valueMapper: WdlValue => WdlValue): String

  def instantiate(call: Call,
                  parameters: WorkflowCoercedInputs,
                  functions: WdlFunctions[WdlValue],
                  shards: Map[Scatter, Int] = Map.empty[Scatter, Int],
                  valueMapper: WdlValue => WdlValue): String
}
