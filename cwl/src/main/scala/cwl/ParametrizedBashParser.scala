package cwl

import cwl.ParametrizedBashParser.{ScanMark, Token}
import cwl.ParametrizedBashParser.Token.TokenType
import cwl.ParametrizedStringTemplate.{CharElement, Element, ParameterPart}

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

  case class ScanResult(stringTemplate: ParametrizedStringTemplate[XRP], scanMarks: Seq[ScanMark[XRP]])

  def scan(parts: Seq[RP]): ScanResult = {
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

  private def newToken(string: ParametrizedStringTemplate[XRP], tokenType: TokenType,
                       beginIndex: Int, endIndex: Int): Token[XRP] = {
    val tokenString = string.substring(beginIndex, endIndex)
    Token(tokenType, beginIndex, tokenString)
  }

  case class TokenizeResult(tokens: Seq[Token[XRP]], nonBlankTokens: Seq[Token[XRP]])

  def tokenize(scanResult: ScanResult): TokenizeResult = {
    var tokens: Seq[Token[XRP]] = Seq.empty
    val scanMarks = scanResult.scanMarks
    val string = scanResult.stringTemplate
    if (scanMarks.nonEmpty) {
      var tokenBeginIndex = 0
      var tokenType = scanMarks.head.tokenType
      for (index <- scanMarks.indices) {
        val tokenTypeNew = scanMarks(index).tokenType
        if (tokenTypeNew != tokenType) {
          val tokenEndIndex = index
          tokens :+= newToken(string, tokenType, tokenBeginIndex, tokenEndIndex)
          tokenType = tokenTypeNew
          tokenBeginIndex = tokenEndIndex
        }
      }
      tokens :+= newToken(string, tokenType, tokenBeginIndex, string.length)
    }
    val nonBlankTokens: Seq[Token[XRP]] = tokens.filterNot(_.tokenType == TokenType.blank)
    TokenizeResult(tokens, nonBlankTokens)
  }

  def tokenize(parts: Seq[RP]): TokenizeResult = tokenize(scan(parts))
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
    def tokenType: TokenType

    def element: Element[P]

    def nextFor(element: Element[P]): ScanMark[P] = element match {
      case ParameterPart(parameter) => nextForParameter(parameter)
      case CharElement(char) => nextForChar(char)
    }

    def nextForParameter(parameter: P): ParameterScanMark[P]

    def nextForChar(char: Char): CharScanMark[P]
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
    override def nextForParameter(parameter: P): ParameterScanMark[P] = ScanMark.startForParameter(parameter)

    override def nextForChar(char: Char): CharScanMark[P] = ScanMark.startForChar[P](char)

    override def tokenType: TokenType = TokenType.blank
  }

  sealed trait EscapeStart[P] extends CharScanMark[P]

  sealed trait Word[P] extends ScanMark[P] {
    override def tokenType: TokenType = TokenType.word
  }

  sealed trait WordNotEscapeStart[P] extends Word[P] {
    override def nextForParameter(parameter: P): ParameterScanMark[P] =
      WordContinuationParameter[P](parameter)

    override def nextForChar(char: Char): CharScanMark[P] = char match {
      case SpecialChars.singleQuote => SingleQuoteStringStart[P](char)
      case SpecialChars.doubleQuote => DoubleQuoteStringStart[P](char)
      case SpecialChars.backslash => WordContinuationEscapeStart(char)
      case _ if char.isWhitespace => Blank[P](char)
      case _ => WordContinuationChar(char)
    }
  }

  sealed trait Escaped[P] extends ScanMark[P]

  sealed trait WordStart[P] extends Word[P]

  sealed trait WordStartNotEscapeStart[P] extends WordStart[P] with WordNotEscapeStart[P]

  case class WordStartChar[P](char: Char) extends WordStartNotEscapeStart[P] with CharScanMark[P]

  case class WordStartEscapeStart[P](char: Char) extends EscapeStart[P] with WordStart[P] {
    override def nextForParameter(parameter: P): ParameterScanMark[P] =
      WordContinuationEscapedParameter[P](parameter)

    override def nextForChar(char: Char): CharScanMark[P] = char match {
      case SpecialChars.lineFeed => Blank[P](char)
      case _ => WordContinuationEscapedChar(char)
    }
  }

  case class WordStartParameter[P](parameter: P)
    extends WordStartNotEscapeStart[P] with ParameterScanMark[P]

  sealed trait WordContinuation[P] extends Word[P]

  sealed trait WordContinuationNotEscapeStart[P] extends WordContinuation[P] with WordNotEscapeStart[P]

  case class WordContinuationChar[P](char: Char)
    extends WordContinuationNotEscapeStart[P] with CharScanMark[P]

  case class WordContinuationParameter[P](parameter: P)
    extends WordContinuationNotEscapeStart[P] with ParameterScanMark[P]

  case class WordContinuationEscapeStart[P](char: Char) extends WordContinuation[P] with EscapeStart[P] {
    override def nextForParameter(parameter: P): ParameterScanMark[P] =
      WordContinuationEscapedParameter(parameter)

    override def nextForChar(char: Char): CharScanMark[P] = char match {
      case SpecialChars.lineFeed => Blank(char)
      case _ => WordContinuationEscapedChar(char)
    }
  }

  sealed trait WordContinuationEscaped[P] extends WordContinuationNotEscapeStart[P] with Escaped[P]

  case class WordContinuationEscapedChar[P](char: Char)
    extends WordContinuationEscaped[P] with CharScanMark[P]

  case class WordContinuationEscapedParameter[P](parameter: P)
    extends WordContinuationEscaped[P] with ParameterScanMark[P]

  sealed trait CommentMark[P] extends ScanMark[P] {
    override def tokenType: TokenType = TokenType.comment

    override def nextForParameter(parameter: P): ParameterScanMark[P] = CommentParameter(parameter)

    override def nextForChar(char: Char): CharScanMark[P] = CommentChar(char)
  }

  case class CommentChar[P](char: Char) extends CharScanMark[P] with CommentMark[P]

  case class CommentParameter[P](parameter: P) extends ParameterScanMark[P] with CommentMark[P]

  sealed trait StringMark[P] extends ScanMark[P] {
    def quotationChar: Char
  }

  sealed trait SingleQuoteString[P] extends StringMark[P] {
    override def tokenType: TokenType = TokenType.singleQuoteString

    override val quotationChar: Char = SpecialChars.singleQuote
  }

  sealed trait DoubleQuoteString[P] extends StringMark[P] {
    override def tokenType: TokenType = TokenType.doubleQuoteString

    override val quotationChar: Char = SpecialChars.doubleQuote
  }

  sealed trait StringStart[P] extends CharScanMark[P] with StringMark[P]

  sealed trait StringEnd[P] extends CharScanMark[P] with StringMark[P] {
    override def nextForParameter(parameter: P): ParameterScanMark[P] = ScanMark.startForParameter(parameter)

    override def nextForChar(char: Char): CharScanMark[P] = ScanMark.startForChar(char)
  }

  trait SingleQuoteStringNonEnd[P] extends SingleQuoteString[P] {
    override def nextForParameter(parameter: P): ParameterScanMark[P] =
      SingleQuoteStringInnerParameter(parameter)

    override def nextForChar(char: Char): CharScanMark[P] = char match {
      case SpecialChars.singleQuote => SingleQuoteStringEnd(char)
      case _ => SingleQuoteStringInnerChar(char)
    }
  }

  case class SingleQuoteStringStart[P](char: Char)
    extends StringStart[P] with SingleQuoteStringNonEnd[P]

  sealed trait SingleQuoteStringInner[P] extends SingleQuoteStringNonEnd[P]

  case class SingleQuoteStringInnerChar[P](char: Char)
    extends SingleQuoteStringInner[P] with CharScanMark[P]

  case class SingleQuoteStringInnerParameter[P](parameter: P)
    extends SingleQuoteStringInner[P] with ParameterScanMark[P]

  case class SingleQuoteStringEnd[P](char: Char) extends SingleQuoteString[P] with StringEnd[P]

  sealed trait DoubleQuoteStringNonEnd[P] extends DoubleQuoteString[P]

  sealed trait DoubleQuoteStringNonEndNoEscape[P] extends DoubleQuoteStringNonEnd[P] {
    override def nextForParameter(parameter: P): ParameterScanMark[P] =
      DoubleQuoteStringInnerParameterNoEscape(parameter)

    override def nextForChar(char: Char): CharScanMark[P] = char match {
      case SpecialChars.doubleQuote => DoubleQuoteStringEnd(char)
      case SpecialChars.backslash => DoubleQuoteStringInnerEscapeStart(char)
      case _ => DoubleQuoteStringInnerCharNoEscape(char)
    }

  }

  case class DoubleQuoteStringStart[P](char: Char)
    extends StringStart[P] with DoubleQuoteStringNonEndNoEscape[P]


  sealed trait DoubleQuoteStringInner[P] extends DoubleQuoteStringNonEnd[P]

  case class DoubleQuoteStringInnerEscapeStart[P](char: Char)
    extends DoubleQuoteStringInner[P] with CharScanMark[P] {
    override def nextForParameter(parameter: P): ParameterScanMark[P] =
      DoubleQuoteStringInnerEscapedParameter(parameter)

    override def nextForChar(char: Char): CharScanMark[P] =
      DoubleQuoteStringInnerEscapedChar(char)
  }

  sealed trait DoubleQuoteStringInnerEscaped[P] extends DoubleQuoteStringInner[P]

  case class DoubleQuoteStringInnerCharNoEscape[P](char: Char)
    extends DoubleQuoteStringInner[P] with CharScanMark[P] with DoubleQuoteStringNonEndNoEscape[P]

  case class DoubleQuoteStringInnerParameterNoEscape[P](parameter: P)
    extends DoubleQuoteStringInner[P] with ParameterScanMark[P] with DoubleQuoteStringNonEndNoEscape[P]

  case class DoubleQuoteStringInnerEscapedChar[P](char: Char)
    extends DoubleQuoteStringInnerEscaped[P] with CharScanMark[P] with DoubleQuoteStringNonEndNoEscape[P]

  case class DoubleQuoteStringInnerEscapedParameter[P](parameter: P)
    extends DoubleQuoteStringInnerEscaped[P] with ParameterScanMark[P] with DoubleQuoteStringNonEndNoEscape[P]

  case class DoubleQuoteStringEnd[P](char: Char) extends DoubleQuoteString[P] with StringEnd[P]

  object ScanMark {
    def startFor[P](element: Element[P]): ScanMark[P] = {
      element match {
        case ParameterPart(parameter) => startForParameter(parameter)
        case CharElement(char) => startForChar[P](char)
      }
    }

    def startForParameter[P](parameter: P): WordStartParameter[P] = WordStartParameter[P](parameter)

    def startForChar[P](char: Char): CharScanMark[P] = char match {
      case SpecialChars.hash => CommentChar[P](char)
      case SpecialChars.singleQuote => SingleQuoteStringStart[P](char)
      case SpecialChars.doubleQuote => DoubleQuoteStringStart[P](char)
      case SpecialChars.backslash => WordStartEscapeStart[P](char)
      case _ if char.isWhitespace => Blank[P](char)
      case _ => WordStartChar(char)
    }
  }

  object Token {

    class TokenType

    object TokenType {
      case object blank extends TokenType
      case object word extends TokenType
      case object comment extends TokenType
      case object singleQuoteString extends TokenType
      case object doubleQuoteString extends TokenType
    }

  }

  case class Token[P](tokenType: TokenType, index: Int, string: ParametrizedStringTemplate[P]) {
    def length: Int = string.length
  }

}