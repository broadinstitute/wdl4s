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

  case class ScanMark[P](element: Element[P], isComment: Boolean, isSingleQuotedString: Boolean,
                         isDoubleQuotedString: Boolean, isEscapingNext: Boolean, isEscaped: Boolean) {
    def nextFor(element: Element[P]): ScanMark[P] = {
      element match {
        case parameterPart: ParameterPart[P] => ???
        case CharElement(char) => ???
      }
    }
  }

  object ScanMark {
    def startFor[P](element: Element[P]): ScanMark[P] = {
      element match {
        case parameterPart: ParameterPart[P] => ???
        case CharElement(char) => ???
      }
    }
  }

  case class ScanResult[P](stringTemplate: ParametrizedStringTemplate[P], scanMarks: Seq[ScanMark[P]])

}