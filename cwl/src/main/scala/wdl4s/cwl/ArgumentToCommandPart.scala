package wdl4s.cwl

import shapeless._
import wdl4s.wdl.command.StringCommandPart
import wdl4s.wom.WomCommandPart

object ArgumentToCommandPart extends Poly1 {
  implicit def expr = at[ECMAScriptExpression] { _ => StringCommandPart(null): WomCommandPart }

  implicit def clb = at[CommandLineBinding] {
      //TODO: This option.get will not hold up under scrutiny
      _.valueFrom.map(_.fold(StringOrExpressionToCommandPart)).get: WomCommandPart

      //TODO: Shell Quote = false?
  }

  implicit def string = at[String] { expr =>
    StringCommandPart(expr): WomCommandPart
  }
}
