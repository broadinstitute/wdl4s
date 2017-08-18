package wdl4s.cwl

import wdl4s.cwl.ParametrizedStringTemplate.{CharElement, Element, ParameterPart}
import wdl4s.cwl.PartialBashTemplateParser.{ParseResult, PosMark}

class PartialBashTemplateParser[RP, SRP <: RP, XRP <: RP](isRawStringPart: RP => Boolean,
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

  def parse(parts: Seq[RP]): ParseResult[XRP] = {
    val stringTemplate = partsToStringTemplate(parts)
    var posMarks: Seq[PosMark[XRP]] = Seq.empty
    if (stringTemplate.length > 0) {
      var posMark = PosMark.startFor[XRP](stringTemplate.charAt(0))
      posMarks :+= posMark
      for (pos <- 1 until stringTemplate.length) {
        posMark = posMark.nextFor(stringTemplate.charAt(pos))
        posMarks :+= posMark
      }
    }
    ParseResult(stringTemplate, posMarks)
  }
}

object PartialBashTemplateParser {

  case class PosMark[P](element: Element[P], isComment: Boolean, isSingleQuotedString: Boolean,
                        isDoubleQuotedString: Boolean, isEscapingNext: Boolean, isEscaped: Boolean) {
    def nextFor(element: Element[P]): PosMark[P] = {
      element match {
        case parameterPart: ParameterPart[P] => ???
        case CharElement(char) => ???
      }
    }
  }

  object PosMark {
    def startFor[P](element: Element[P]): PosMark[P] = {
      element match {
        case parameterPart: ParameterPart[P] => ???
        case CharElement(char) => ???
      }
    }
  }

  case class ParseResult[P](stringTemplate: ParametrizedStringTemplate[P], posMarks: Seq[PosMark[P]])

}