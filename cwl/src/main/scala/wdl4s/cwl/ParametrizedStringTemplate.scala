package wdl4s.cwl

import wdl4s.cwl.ParametrizedStringTemplate._

case class ParametrizedStringTemplate[P](parts: Seq[Part[P]], lengthSums: Seq[Int]) {

  def +(string: String): ParametrizedStringTemplate[P] = {
    if (string.nonEmpty) {
      if (parts.nonEmpty && parts.last.isInstanceOf[StringPart]) {
        val newLengthSums = lengthSums.dropRight(1) :+ (lengthSums.last + string.length)
        val lastStringPart = parts.last.asInstanceOf[StringPart]
        val newStringPart = StringPart(lastStringPart.string + string)
        ParametrizedStringTemplate[P](parts.dropRight(1) :+ newStringPart, newLengthSums)
      } else {
        val newLastLength = lengthSums.lastOption.getOrElse(0) + string.length
        ParametrizedStringTemplate[P](parts :+ StringPart(string), lengthSums :+ newLastLength)
      }
    } else {
      this
    }
  }

  def +(parameter: P): ParametrizedStringTemplate[P] = {
    val newParts = parts :+ ParameterPart[P](parameter)
    val newLengths = lengthSums :+ (lengthSums.last + 1)
    ParametrizedStringTemplate[P](newParts, newLengths)
  }

  def length: Int = lengthSums.lastOption.getOrElse(0)

  def partOffset(iPart: Int) = {
    if (iPart == 0) {
      0
    } else {
      lengthSums(iPart - 1)
    }
  }

  def toString(paramToString: P => String): String = parts.map {
    case StringPart(string) => string
    case ParameterPart(parameter) => paramToString(parameter)
  }.mkString("")

  def charAt(pos: Int): Element[P] = {
    if (pos < 0) throw new IndexOutOfBoundsException(s"Pos $pos must not be negative.")
    val iPart = lengthSums.indexWhere(pos < _)
    if (iPart < 0) throw new IndexOutOfBoundsException(s"Pos $pos too large for length $length")
    parts(iPart) match {
      case parameterPart: ParameterPart[P] => parameterPart
      case StringPart(string) => CharElement(string.charAt(pos - partOffset(iPart)))
    }
  }

  def indexOf[S](sub: S, fromIndex: Int = 0)(subIndex: (String, S, Int) => Int): Int = {
    var indexOpt: Option[Int] = None
    var iPart = 0
    while (indexOpt.isEmpty && iPart < parts.size) {
      parts(iPart) match {
        case StringPart(string) =>
          if (fromIndex < lengthSums(iPart)) {
            val offset = partOffset(iPart)
            val partFromIndex = if (fromIndex > offset) fromIndex - offset else 0
            val partIndexOf = subIndex(string, sub, partFromIndex)
            if (partIndexOf > -1) {
              indexOpt = Option(partIndexOf + offset)
            }
          }
        case _ => ()
      }
      iPart += 1
    }
    indexOpt match {
      case Some(index) => index
      case None => -1
    }
  }

  def indexOfChar(ch: Int, fromIndex: Int = 0): Int =
    indexOf[Int](ch, fromIndex)((string: String, ch: Int, fromIndex: Int) => string.indexOf(ch, fromIndex))

  def indexOfStr(str: String, fromIndex: Int = 0): Int =
    indexOf[String](str, fromIndex)(
      (string: String, str: String, fromIndex: Int) => string.indexOf(str, fromIndex))

}

object ParametrizedStringTemplate {

  sealed trait Part[+P] {
    def length: Int
  }

  sealed trait Element[+P]

  case class ParameterPart[+P](parameter: P) extends Part[P] with Element[P] {
    override def length: Int = 1
  }

  case class StringPart(string: String) extends Part[Nothing] {
    override def length: Int = string.length
  }

  case class CharElement(char: Char) extends Element[Nothing]

  def empty[P]: ParametrizedStringTemplate[P] = ParametrizedStringTemplate(Seq.empty, Seq.empty)

  def fromString[P](string: String): ParametrizedStringTemplate[P] =
    ParametrizedStringTemplate(Seq(StringPart(string)), Seq(string.length))

  def fromParameter[P](parameter: P): ParametrizedStringTemplate[P] =
    ParametrizedStringTemplate[P](Seq(ParameterPart[P](parameter)), Seq(1))
}