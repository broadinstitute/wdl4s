package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.cwl.ParametrizedBashParser.Token.TokenType

class ParametrizedBashParserSpec extends FlatSpec with Matchers {

  sealed trait MockPart

  case class MockStringPart(string: String) extends MockPart

  case class MockExpressionPart(expr: String) extends MockPart

  def msp(string: String): MockStringPart = MockStringPart(string)

  def mep(expr: String): MockExpressionPart = MockExpressionPart(expr)

  def newParser: ParametrizedBashParser[MockPart, MockStringPart, MockExpressionPart] =
    new ParametrizedBashParser[MockPart, MockStringPart,
      MockExpressionPart](_.isInstanceOf[MockStringPart], _.string)

  it should "merge adjacent string tokens" in {
    val parser = newParser
    val parts = Seq(msp("The"), msp(" "), msp("answer"), msp(" "), msp("is"), msp(" "), mep("42"), msp("."),
      msp("he said after "), mep("7"), mep(" "), mep("years"), msp("."))
    val stringTemplate = parser.partsToStringTemplate(parts)
    stringTemplate.length shouldBe 34
  }

  it should "properly tokenize command lines" in {
    val (bl, wo, co, sq, dq) = {
      import TokenType._
      (blank, word, comment, singleQuoteString, doubleQuoteString)
    }
    val parser = newParser
    def tokenTypes(parts: Seq[MockPart]): Seq[TokenType] = parser.tokenize(parts).map(_.tokenType)
    tokenTypes(Seq(msp("echo Hello World"))) shouldBe Seq(wo, bl, wo, bl, wo)
    tokenTypes(Seq(msp("echo 'Hello' \"World\""))) shouldBe Seq(wo, bl, sq, bl, dq)
    tokenTypes(Seq(msp("echo 'Hello' \"World\" # It is saying \"Hello World\""))) shouldBe
      Seq(wo, bl, sq, bl, dq, bl, co)
    tokenTypes(Seq(msp("echo \"Hello 'World'\""))) shouldBe Seq(wo, bl, dq)
    tokenTypes(Seq(msp("echo Hello "), mep("World"))) shouldBe Seq(wo, bl, wo, bl, wo)
    tokenTypes(Seq(msp("echo 'Hello' \""), mep("World"), msp("\""))) shouldBe Seq(wo, bl, sq, bl, dq)
    tokenTypes(Seq(msp("echo 'Hello' \""), mep("World"), msp("\" # It is saying \""),
      mep("Hello World"), msp("\""))) shouldBe
      Seq(wo, bl, sq, bl, dq, bl, co)
    tokenTypes(Seq(msp("echo \"Hello '"), mep("World"), msp("'\""))) shouldBe Seq(wo, bl, dq)
  }

}
