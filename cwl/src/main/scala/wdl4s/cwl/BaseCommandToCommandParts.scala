package wdl4s.cwl

import shapeless.Poly1
import wdl4s.wdl.command.StringCommandPart
import wdl4s.wom.WomCommandPart

object BaseCommandToCommandParts extends Poly1 {
  implicit def one = at[String] { baseCommand => Seq(baseCommand).map(StringCommandPart.apply): Seq[WomCommandPart] }

  implicit def many = at[Array[String]] { _.toSeq.map(StringCommandPart.apply): Seq[WomCommandPart] }
}
