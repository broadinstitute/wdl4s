package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}

class ParametrizedBashParserSpec extends FlatSpec with Matchers {

  sealed trait MockPart

  case class MockStringPart(string: String) extends MockPart

  case class MockExpressionPart(expr: String) extends MockPart

  def msp(string: String): MockStringPart = MockStringPart(string)

  def mep(expr: String): MockExpressionPart = MockExpressionPart(expr)

  it should "merge adjacent string tokens" in {
    val parser =
      new ParametrizedBashParser[MockPart, MockStringPart,
        MockExpressionPart](_.isInstanceOf[MockStringPart], _.string)
    val parts = Seq(msp("The"), msp(" "), msp("answer"), msp(" "), msp("is"), msp(" "), mep("42"), msp("."),
      msp("he said after "), mep("7"), mep(" "), mep("years"), msp("."))
    val stringTemplate = parser.partsToStringTemplate(parts)
    stringTemplate.length shouldBe 34
  }

  it should "properly scan command lines" in {

  }

}
