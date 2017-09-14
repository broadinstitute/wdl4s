package wdl4s.cwl

import shapeless.Poly1
import wdl4s.wdl.command.StringCommandPart
import wdl4s.wom.CommandPart

object BaseCommandToCommandParts extends Poly1 {
  implicit def one = at[String] { Seq(_).map(StringCommandPart.apply): Seq[CommandPart] }

  implicit def many = at[Array[String]] { _.toSeq.map(StringCommandPart.apply): Seq[CommandPart] }
}
