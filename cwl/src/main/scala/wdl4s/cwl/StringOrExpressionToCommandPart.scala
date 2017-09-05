package wdl4s.cwl

import shapeless._
import wdl4s.wdl.command.StringCommandPart
import wdl4s.wom.WomCommandPart

object StringOrExpressionToCommandPart extends Poly1 {
  val EcmaScriptRegex = """\$\(([^)]*)\)""".r
  implicit def expression = at[ECMAScriptExpression] { ex => CwlExpressionCommandPart(ex.value): WomCommandPart }

  implicit def string = at[String] {
    case EcmaScriptRegex(expr) => CwlExpressionCommandPart(expr): WomCommandPart
    case part => StringCommandPart(part): WomCommandPart
  }
}
