package wdl4s.cwl

class PartialBashTemplateParser[RP, SRP <: RP, XRP <: RP](isRawStringPart: RP => Boolean,
                                                          rawPartToString: SRP => String) {

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
      if(isRawStringPart(rawPart)) {
        StringRawPartToken(rawPart.asInstanceOf[SRP])
      } else {
        ExpressionRawPartToken(rawPart.asInstanceOf[XRP])
      }
    }

  def mergeStrings(rawPartTokens: Seq[RawPartToken]): Seq[RawToken] = {
    var tokens : Seq[RawToken] = Seq.empty
    var stringTokenQueue: Seq[StringRawPartToken] = Seq.empty
    def flushStringTokenQueue(): Unit = {
      if(stringTokenQueue.nonEmpty) {
        tokens :+= CombinedStringToken(stringTokenQueue)
        stringTokenQueue = Seq.empty
      }
    }
    for(rawPartToken <- rawPartTokens) {
      rawPartToken match {
        case stringToken: StringRawPartToken => stringTokenQueue :+= stringToken
        case expressionToken: ExpressionRawPartToken =>
          flushStringTokenQueue()
          tokens :+= ExpressionRawPartToken
      }
    }
    flushStringTokenQueue()
    tokens
  }

}
