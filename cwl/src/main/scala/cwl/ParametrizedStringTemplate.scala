package cwl

import cwl.ParametrizedStringTemplate.{CharElement, Element, ParameterPart, Part, StringPart}

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
    val newLengths = lengthSums :+ (lengthSums.lastOption.getOrElse(0) + 1)
    ParametrizedStringTemplate[P](newParts, newLengths)
  }

  def +(part: Part[P]): ParametrizedStringTemplate[P] = part match {
    case ParameterPart(parameter) => this + parameter
    case StringPart(string) => this + string
  }

  def length: Int = lengthSums.lastOption.getOrElse(0)

  def partOffset(iPart: Int): Int = if (iPart == 0) 0 else lengthSums(iPart - 1)

  case class Pos(index: Int, iPart: Int, partIndex: Int)

  object Pos {
    def fromIndex(index: Int): Pos = {
      val iPart = if (index < length) lengthSums.indexWhere(index < _) else parts.size - 1
      val partIndex = index - partOffset(iPart)
      Pos(index, iPart, partIndex)
    }

    def fromPartIndex(partIndex: Int, iPart: Int): Pos = {
      val index = partIndex + partOffset(iPart)
      Pos(index, iPart, partIndex)
    }
  }


  def toString(paramToString: P => String): String = parts.map {
    case StringPart(string) => string
    case ParameterPart(parameter) => paramToString(parameter)
  }.mkString("")

  def toStringOption: Option[String] = {
    if (parts.forall(_.isInstanceOf[StringPart])) {
      Option(parts.collect({
        case StringPart(string) => string
      }).mkString(""))
    } else {
      None
    }
  }

  def charAt(index: Int): Element[P] = {
    if (index < 0) throw new IndexOutOfBoundsException(s"Index $index must not be negative.")
    val pos = Pos.fromIndex(index)
    if (index >= length) throw new IndexOutOfBoundsException(s"Index $index too large for length $length")
    parts(pos.iPart) match {
      case parameterPart: ParameterPart[P] => parameterPart
      case StringPart(string) => CharElement(string.charAt(pos.partIndex))
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

  def substring(beginIndex: Int, endIndex: Int = length): ParametrizedStringTemplate[P] = {
    if (beginIndex < 0) throw new IndexOutOfBoundsException(s"Begin index $beginIndex is less than zero.")
    if (endIndex < beginIndex)
      throw new IndexOutOfBoundsException(s"End index $endIndex is less than begin index $beginIndex.")
    if (endIndex > length)
      throw new IndexOutOfBoundsException(s"End index $endIndex is greater than length $length.")
    val beginPos = Pos.fromIndex(beginIndex)
    val endPos = Pos.fromIndex(endIndex)
    var substring = ParametrizedStringTemplate.empty[P]
    if (beginPos.index < endPos.index) {
      for (iPart <- beginPos.iPart to endPos.iPart) {
        val partBeginIndex = if (iPart == beginPos.iPart) beginPos.partIndex else 0
        val partEndIndex = if (iPart == endPos.iPart) endPos.partIndex else parts(iPart).length
        val partSub = parts(iPart).subPart(partBeginIndex, partEndIndex)
        if (partSub.nonEmpty) substring += partSub
      }
    }
    substring
  }

  def hasParameters: Boolean = parts.exists(_.isParameter)

  def nParameters: Int = parts.count(_.isParameter)

  def parameters: Seq[P] = parts.collect({ case ParameterPart(parameter) => parameter })

  def isBareParameter: Boolean = parts.size == 1 && parts.head.isParameter

  def isPrefixedParameter: Boolean = parts.size == 2 && !parts.head.isParameter && parts(1).isParameter

  def prefix: String = if (parts.isEmpty || parts.head.isParameter) {
    ""
  } else {
    parts.head.asInstanceOf[StringPart].string
  }
}

object ParametrizedStringTemplate {

  sealed trait Part[+P] {
    def length: Int

    def nonEmpty: Boolean

    def isParameter: Boolean

    def subPart(beginIndex: Int, endIndex: Int = length): Part[P]
  }

  sealed trait Element[+P]

  case class ParameterPart[+P](parameter: P) extends Part[P] with Element[P] {
    override def length: Int = 1

    override def nonEmpty: Boolean = true

    override def isParameter: Boolean = true

    override def subPart(beginIndex: Int, endIndex: Int): Part[P] = {
      if (beginIndex < 0)
        throw new IndexOutOfBoundsException(s"Begin index $beginIndex must not be less than zero")
      if (endIndex > 1)
        throw new IndexOutOfBoundsException(s"End index $endIndex must not be greater than one")
      if (beginIndex > endIndex)
        throw new IndexOutOfBoundsException(
          s"Begin index $beginIndex must not be greater than end index $endIndex.")
      if (beginIndex < endIndex) this else StringPart("")
    }
  }

  case class StringPart(string: String) extends Part[Nothing] {
    override def length: Int = string.length

    override def nonEmpty: Boolean = string.nonEmpty

    override def isParameter: Boolean = false

    override def subPart(beginIndex: Int, endIndex: Int): Part[Nothing] =
      StringPart(string.substring(beginIndex, endIndex))
  }

  case class CharElement(char: Char) extends Element[Nothing]

  def empty[P]: ParametrizedStringTemplate[P] = ParametrizedStringTemplate(Seq.empty, Seq.empty)

  def fromString[P](string: String): ParametrizedStringTemplate[P] =
    ParametrizedStringTemplate(Seq(StringPart(string)), Seq(string.length))

  def fromParameter[P](parameter: P): ParametrizedStringTemplate[P] =
    ParametrizedStringTemplate[P](Seq(ParameterPart[P](parameter)), Seq(1))
}