package wdl4s.cwl

import shapeless._
import wdl4s.wdl.command.StringCommandPart
import wdl4s.wom.CommandPart

object ArgumentToCommandPart extends Poly1 {
  implicit def expr = at[ECMAScriptExpression] { _ => StringCommandPart(null): CommandPart }

  implicit def clb = at[CommandLineBinding] {
      //TODO: This option.get will not hold up under scrutiny
      _.valueFrom.map(_.fold(StringOrExpressionToCommandPart)).get: CommandPart

      //TODO: Shell Quote = false?
  }

  implicit def string = at[String] { expr =>
    StringCommandPart(expr): CommandPart
  }
}
