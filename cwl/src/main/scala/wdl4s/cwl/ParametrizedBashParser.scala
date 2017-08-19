package wdl4s.cwl

import wdl4s.cwl.ParametrizedStringTemplate.{CharElement, Element, ParameterPart}
import wdl4s.cwl.ParametrizedBashParser.{ScanResult, ScanMark}

class ParametrizedBashParser[RP, SRP <: RP, XRP <: RP](isRawStringPart: RP => Boolean,
                                                       rawPartToString: SRP => String) {

  def partsToStringTemplate(parts: Seq[RP]): ParametrizedStringTemplate[XRP] = {
    var stringTemplate = ParametrizedStringTemplate.empty[XRP]
    for (part <- parts) {
      if (isRawStringPart(part)) {
        stringTemplate += rawPartToString(part.asInstanceOf[SRP])
      } else {
        stringTemplate += part.asInstanceOf[XRP]
      }
    }
    stringTemplate
  }

  def scan(parts: Seq[RP]): ScanResult[XRP] = {
    val stringTemplate = partsToStringTemplate(parts)
    var scanMarks: Seq[ScanMark[XRP]] = Seq.empty
    if (stringTemplate.length > 0) {
      var scanMark = ScanMark.startFor[XRP](stringTemplate.charAt(0))
      scanMarks :+= scanMark
      for (pos <- 1 until stringTemplate.length) {
        scanMark = scanMark.nextFor(stringTemplate.charAt(pos))
        scanMarks :+= scanMark
      }
    }
    ScanResult(stringTemplate, scanMarks)
  }
}

object ParametrizedBashParser {

  object SpecialChars {
    val hash: Char = '#'
    val singleQuote: Char = '\''
    val doubleQuote: Char = '"'
    val backslash: Char = '\\'
  }

  sealed trait ScanMark[P] {
    def element: Element[P]

    def nextFor(element: Element[P]): ScanMark[P]
  }

  sealed trait CharScanMark[P] extends ScanMark[P] {
    def char: Char

    override def element: CharElement = CharElement(char)
  }

  sealed trait ParameterScanMark[P] extends ScanMark[P] {
    def parameter: P

    override def element: ParameterPart[P] = ParameterPart[P](parameter)
  }

  case class Blank[P](char: Char) extends CharScanMark[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  sealed trait EscapeStart[P] extends CharScanMark[P]

  sealed trait Escaped[P] extends CharScanMark[P]

  sealed trait WordStart[P] extends ScanMark[P]

  case class WordStartChar[P](char: Char) extends WordStart[P] {
    override def element: Element[P] = CharElement(char: Char)

    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  case class WordStartEscapeStart[P](char: Char) extends EscapeStart[P] with WordStart[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  sealed trait WordContinuation[P] extends CharScanMark[P]

  case class WordContinuationChar[P](char: Char) extends WordContinuation[P] with CharScanMark[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }
  case class WordContinuationEscapeStart[P](char: Char) extends WordContinuation[P] with EscapeStart[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  case class WordContinuationEscapedChar[P](char: Char) extends WordContinuation[P] with Escaped[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  case class ParameterWord[P](parameter: P) extends ParameterScanMark[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  sealed trait CommentMark[P] extends ScanMark[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  case class CommentChar[P](char: Char) extends CharScanMark[P] with CommentMark[P]

  case class CommentParameter[P](parameter: P) extends ParameterScanMark[P] with CommentMark[P]

  sealed trait StringMark[P] extends ScanMark[P] {
    def quotationChar: Char
  }

  sealed trait SingleQuoteString[P] extends StringMark[P] {
    override val quotationChar : Char = SpecialChars.singleQuote
  }

  sealed trait DoubleQuoteString[P] extends StringMark[P] {
    override val quotationChar : Char = SpecialChars.doubleQuote
  }

  sealed trait StringStart[P] extends CharScanMark[P] with StringMark[P]
  sealed trait StringEnd[P] extends CharScanMark[P] with StringMark[P]

  case class SingleQuoteStringStart[P](char:Char) extends StringStart[P] with SingleQuoteString[P]{
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  case class DoubleQuoteStringStart[P](char: Char) extends StringStart[P] with DoubleQuoteString[P]{
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  sealed trait SingleQuoteStringInner[P] extends SingleQuoteString[P] {
    override def nextFor(element: Element[P]): SingleQuoteString[P] = ???
  }

  case class SingleQuoteStringInnerChar[P](char: Char)
    extends SingleQuoteStringInner[P] with CharScanMark[P]

  case class SingleQuoteStringInnerParameter[P](parameter: P)
    extends SingleQuoteStringInner[P] with ParameterScanMark[P]

  sealed trait DoubleQuoteStringInner[P] extends DoubleQuoteString[P]{
    override def nextFor(element: Element[P]): DoubleQuoteString[P] = ???
  }

  case class DoubleQuoteStringInnerEscapeStart[P](char: Char)
    extends DoubleQuoteStringInner[P] with CharScanMark[P]

  sealed trait DoubleQuoteStringInnerEscaped[P] extends DoubleQuoteStringInner[P]

  case class DoubleQuoteStringInnerChar[P](char: Char)
    extends DoubleQuoteStringInner[P] with CharScanMark[P]

  case class DoubleQuoteStringInnerParameter[P](parameter: P)
    extends DoubleQuoteStringInner[P] with ParameterScanMark[P]

  case class DoubleQuoteStringInnerEscapedChar[P](char: Char)
    extends DoubleQuoteStringInnerEscaped[P] with CharScanMark[P]

  case class DoubleQuoteStringInnerEscapedParameter[P](parameter: P)
    extends DoubleQuoteStringInnerEscaped[P] with ParameterScanMark[P]

  object ScanMark {
    def startFor[P](element: Element[P]): ScanMark[P] = {
      element match {
        case ParameterPart(parameter) => ParameterWord[P](parameter)
        case CharElement(char) => char match {
          case SpecialChars.hash => CommentChar[P](char)
          case SpecialChars.singleQuote => SingleQuoteStringStart[P](char)
          case SpecialChars.doubleQuote => DoubleQuoteStringStart[P](char)
          case SpecialChars.backslash => WordStartEscapeStart[P](char)
          case _ if char.isWhitespace => Blank[P](char)
          case _ => WordStartChar(char)
        }
      }
    }
  }

  case class ScanResult[P](stringTemplate: ParametrizedStringTemplate[P], scanMarks: Seq[ScanMark[P]])

}