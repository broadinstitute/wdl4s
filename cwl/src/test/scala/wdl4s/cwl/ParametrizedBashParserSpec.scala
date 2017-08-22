package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}

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
    val parser = newParser
    val parts = Seq(msp("echo Hello World"))
    val tokens = parser.tokenize(parts)
    println(tokens)
  }

}
