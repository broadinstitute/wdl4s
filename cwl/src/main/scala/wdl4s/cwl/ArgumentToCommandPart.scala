package wdl4s.cwl

import shapeless._
import wdl4s.wdl.command.StringCommandPart

object ArgumentToCommandPart extends Poly1 {
  implicit def expr = at[ECMAScriptExpression] {
    expr =>
      StringCommandPart(null)
  }

  implicit def clb = at[CommandLineBinding] {
    clb =>

      //TODO: This option.get will not hold up under scrutiny
      clb.valueFrom.map(_.fold(StringOrExpressionToCommandPart)).get

      //TODO: Shell Quote = false?
  }

  implicit def string = at[String] {
    s => StringCommandPart(s)
  }
}

