package wdl4s.cwl

import wdl4s.cwl.ParametrizedStringTemplate.{CharElement, Element, ParameterPart}
import wdl4s.cwl.PartialBashTemplateParser.{ParseResult, PosMark}

class PartialBashTemplateParser[RP, SRP <: RP, XRP <: RP](isRawStringPart: RP => Boolean,
                                                          rawPartToString: SRP => String) {

  def partsToStringTemplate(parts: Seq[RP]): ParametrizedStringTemplate[XRP] = {
    var stringTemplate = ParametrizedStringTemplate.empty[XRP]
    for(part <- parts) {
      if(isRawStringPart(part)) {
        stringTemplate += rawPartToString(part.asInstanceOf[SRP])
      } else {
        stringTemplate += part.asInstanceOf[XRP]
      }
    }
    stringTemplate
  }

  def scanStep(previousMark: PosMark, element: Element[XRP]): PosMark = {
    element match {
      case ParameterPart(parameter) => ???
      case CharElement(char) => ???
    }
  }

  def parse(parts: Seq[RP]): ParseResult[XRP] = {
    val stringTemplate = partsToStringTemplate(parts)
    var posMarks : Seq[PosMark] = Seq.empty
    for(pos <- 0 until stringTemplate.length) {

    }
    ???
    ParseResult(stringTemplate, posMarks)
  }

  sealed trait RawToken

  sealed trait RawPartToken extends RawToken {
    def raw: RP
  }

  case class ExpressionRawPartToken(raw: XRP) extends RawPartToken

  sealed trait StringToken extends RawToken {
    def string: String
  }

  case class StringRawPartToken(raw: SRP) extends RawPartToken with StringToken {
    override def string: String = rawPartToString(raw)
  }

  case class CombinedStringToken(children: Seq[StringToken]) extends StringToken {
    override def string: String = children.map(_.string).mkString("")
  }

  def rawPartsToTokens(rawParts: Seq[RP]): Seq[RawPartToken] =
    rawParts.map { rawPart: RP =>
      if (isRawStringPart(rawPart)) {
        StringRawPartToken(rawPart.asInstanceOf[SRP])
      } else {
        ExpressionRawPartToken(rawPart.asInstanceOf[XRP])
      }
    }

  def mergeStrings(rawPartTokens: Seq[RawPartToken]): Seq[RawToken] = {
    var tokens: Seq[RawToken] = Seq.empty
    var stringTokenQueue: Seq[StringRawPartToken] = Seq.empty

    def flushStringTokenQueue(): Unit = {
      if (stringTokenQueue.nonEmpty) {
        tokens :+= CombinedStringToken(stringTokenQueue)
        stringTokenQueue = Seq.empty
      }
    }

    for (rawPartToken <- rawPartTokens) {
      rawPartToken match {
        case stringToken: StringRawPartToken => stringTokenQueue :+= stringToken
        case expressionToken: ExpressionRawPartToken =>
          flushStringTokenQueue()
          tokens :+= expressionToken
      }
    }
    flushStringTokenQueue()
    tokens
  }

}

object PartialBashTemplateParser {

  sealed trait PosMark

  case class ExpressionMark(index: Int) extends PosMark

  case class ParseResult[P](stringTemplate: ParametrizedStringTemplate[P], posMarks: Seq[PosMark])

}