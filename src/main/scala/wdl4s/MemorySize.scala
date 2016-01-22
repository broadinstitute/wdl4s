package wdl4s

import wdl4s.parser.MemoryUnit

case class MemorySize(amount: Double, unit: MemoryUnit) {
  def bytes: Double = amount * unit.bytes
  def to(unit: MemoryUnit): MemorySize = MemorySize(this.bytes / unit.bytes, unit)
  override def toString: String = {
    val adjustedAmount = (unit, amount) match {
      case (MemoryUnit.Bytes, a) => a.ceil.toLong.toString
      case (_, a) if a == a.toLong => a.toLong.toString
      case (_, a) => a.toString
    }
    s"$adjustedAmount ${unit.suffixes(0)}"
  }
}
