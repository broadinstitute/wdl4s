package wdl4s.cwl

import wdl4s.wdl.types._
import wdl4s.wdl.values._

object ParameterContext {
  val Empty = ParameterContext(
    inputs = WdlOptionalValue(WdlNothingType, None),
    self = WdlOptionalValue(WdlNothingType, None),
    runtime = WdlOptionalValue(WdlNothingType, None)
  )
}

case class ParameterContext(
                             inputs: WdlValue = WdlOptionalValue(WdlNothingType, None),
                             self: WdlValue = WdlOptionalValue(WdlNothingType, None),
                             runtime: WdlValue = WdlOptionalValue(WdlNothingType, None))
