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
    val lineFeed: Char = '\n'
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
    override def nextFor(element: Element[P]): ScanMark[P] = ScanMark.startFor[P](element)
  }

  sealed trait EscapeStart[P] extends CharScanMark[P]

  sealed trait WordNotEscapeStart[P] extends ScanMark[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = element match {
      case ParameterPart(parameter) => WordContinuationParameter[P](parameter)
      case CharElement(elemChar) => elemChar match {
        case SpecialChars.singleQuote => SingleQuoteStringStart[P](elemChar)
        case SpecialChars.doubleQuote => DoubleQuoteStringStart[P](elemChar)
        case SpecialChars.backslash => WordContinuationEscapeStart(elemChar)
        case _ if elemChar.isWhitespace => Blank[P](elemChar)
        case _ => WordContinuationChar(elemChar)
      }
    }
  }

  sealed trait Escaped[P] extends ScanMark[P]

  sealed trait WordStart[P] extends ScanMark[P]

  sealed trait WordStartNotEscapeStart[P] extends WordStart[P] with WordNotEscapeStart[P]

  case class WordStartChar[P](char: Char) extends WordStartNotEscapeStart[P] with CharScanMark[P]

  case class WordStartEscapeStart[P](char: Char) extends EscapeStart[P] with WordStart[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = element match {
      case ParameterPart(parameter) => WordContinuationEscapedParameter[P](parameter)
      case CharElement(elemChar) => elemChar match {
        case SpecialChars.lineFeed => Blank[P](elemChar)
        case _ => WordContinuationEscapedChar(char)
      }
    }
  }

  case class WordStartParameter[P](parameter: P)
    extends WordStartNotEscapeStart[P] with ParameterScanMark[P]

  sealed trait WordContinuation[P] extends ScanMark[P]

  sealed trait WordContinuationNotEscapeStart[P] extends WordContinuation[P] with WordNotEscapeStart[P]

  case class WordContinuationChar[P](char: Char)
    extends WordContinuationNotEscapeStart[P] with CharScanMark[P]

  case class WordContinuationParameter[P](parameter: P)
    extends WordContinuationNotEscapeStart[P] with ParameterScanMark[P]

  case class WordContinuationEscapeStart[P](char: Char) extends WordContinuation[P] with EscapeStart[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = element match {
      case ParameterPart(parameter) => WordContinuationEscapedParameter(parameter)
      case CharElement(char) => char match {
        case SpecialChars.lineFeed => Blank(char)
        case _ => WordContinuationEscapedChar(char)
      }
    }
  }

  sealed trait WordContinuationEscaped[P] extends WordContinuationNotEscapeStart[P] with Escaped[P]

  case class WordContinuationEscapedChar[P](char: Char)
    extends WordContinuationEscaped[P] with CharScanMark[P]

  case class WordContinuationEscapedParameter[P](parameter: P)
    extends WordContinuationEscaped[P] with ParameterScanMark[P]

  sealed trait CommentMark[P] extends ScanMark[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = element match {
      case ParameterPart(parameter) => CommentParameter(parameter)
      case CharElement(char) => CommentChar(char)
    }
  }

  case class CommentChar[P](char: Char) extends CharScanMark[P] with CommentMark[P]

  case class CommentParameter[P](parameter: P) extends ParameterScanMark[P] with CommentMark[P]

  sealed trait StringMark[P] extends ScanMark[P] {
    def quotationChar: Char
  }

  sealed trait SingleQuoteString[P] extends StringMark[P] {
    override val quotationChar: Char = SpecialChars.singleQuote
  }

  sealed trait DoubleQuoteString[P] extends StringMark[P] {
    override val quotationChar: Char = SpecialChars.doubleQuote
  }

  sealed trait StringStart[P] extends CharScanMark[P] with StringMark[P]

  sealed trait StringEnd[P] extends CharScanMark[P] with StringMark[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ScanMark.startFor(element)
  }

  trait SingleQuoteStringNonEnd[P] extends SingleQuoteString[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = element match {
      case ParameterPart(parameter) => SingleQuoteStringInnerParameter(parameter)
      case CharElement(char) => char match {
        case SpecialChars.singleQuote => SingleQuoteStringEnd(char)
        case _ => SingleQuoteStringInnerChar(char)
      }
    }
  }

  case class SingleQuoteStringStart[P](char: Char)
    extends StringStart[P] with SingleQuoteStringNonEnd[P]

  sealed trait SingleQuoteStringInner[P] extends SingleQuoteString[P] {
    override def nextFor(element: Element[P]): SingleQuoteString[P] = ???
  }

  case class SingleQuoteStringInnerChar[P](char: Char)
    extends SingleQuoteStringInner[P] with CharScanMark[P]

  case class SingleQuoteStringInnerParameter[P](parameter: P)
    extends SingleQuoteStringInner[P] with ParameterScanMark[P]

  case class SingleQuoteStringEnd[P](char: Char) extends SingleQuoteString[P] with StringEnd[P]

  case class DoubleQuoteStringStart[P](char: Char) extends StringStart[P] with DoubleQuoteString[P] {
    override def nextFor(element: Element[P]): ScanMark[P] = ???
  }

  sealed trait DoubleQuoteStringInner[P] extends DoubleQuoteString[P] {
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
        case ParameterPart(parameter) => WordStartParameter[P](parameter)
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