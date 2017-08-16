package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}

class PartialBashTemplateParserSpec  extends FlatSpec with Matchers {

  sealed trait MockPart
  case class MockStringPart(string:String) extends MockPart
  case class MockExpressionPart(expr:String) extends MockPart

  it should "merge adjacent string tokens" in {
    def msp(string:String): MockStringPart = MockStringPart(string)
    def mep(expr:String): MockExpressionPart = MockExpressionPart(expr)
    val parser =
      new PartialBashTemplateParser[MockPart, MockStringPart,
        MockExpressionPart](_.isInstanceOf[MockStringPart], _.string)
    val parts = Seq(msp("The"), msp(" "), msp("answer"), msp(" "), msp("is"), msp(" "), mep("42"), msp("."),
      msp("he said after "), mep("7"), mep(" "), mep("years"), msp("."))
    val rawTokens = parser.rawPartsToTokens(parts)
    val mergedTokens = parser.mergeStrings(rawTokens)
    mergedTokens.size shouldBe 7
    val headToken = mergedTokens.head
    headToken.isInstanceOf[parser.StringToken] shouldBe true
    headToken.asInstanceOf[parser.StringToken].string shouldBe "The answer is "
  }

}
