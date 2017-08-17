package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.cwl.ParametrizedStringTemplate.{CharElement, ParameterPart}

class ParametrizedStringTemplateSpec extends FlatSpec with Matchers {

  case class MockParameter(id: Int)

  val ps: Seq[MockParameter] = (0 until 10).map(MockParameter)

  val pst1 =
    ParametrizedStringTemplate.fromParameter[MockParameter](ps(0)) + "Hello " + "World " + ps(1) + ps(2) +
      "what a day!" + ps(3)
  val pst2 =
    ParametrizedStringTemplate.fromString[MockParameter]("Yo") + ps(5) + ps(6) + " dis" + " gonna" + " be" +
      " great!"

  it should "construct correct parametrized string templates" in {
    pst1.parts.size shouldBe 6
    pst2.parts.size shouldBe 4
    val paraToString = (para: MockParameter) => para.id.toString
    pst1.toString(paraToString) shouldBe "0Hello World 12what a day!3"
    pst2.toString(paraToString) shouldBe "Yo56 dis gonna be great!"
    pst1.lengthSums shouldBe Seq(1, 13, 14, 15, 26, 27)
    pst2.lengthSums shouldBe Seq(2, 3, 4, 24)
  }

  it should "query for chars and substrings" in {
    pst1.charAt(0) shouldBe ParameterPart(ps(0))
    pst1.charAt(1) shouldBe CharElement('H')
    pst1.charAt(2) shouldBe CharElement('e')
    pst1.charAt(6) shouldBe CharElement(' ')
    pst1.charAt(7) shouldBe CharElement('W')
    pst1.charAt(13) shouldBe ParameterPart(ps(1))
    pst1.charAt(14) shouldBe ParameterPart(ps(2))
    pst1.charAt(25) shouldBe CharElement('!')
    pst1.charAt(26) shouldBe ParameterPart(ps(3))
    pst2.charAt(0) shouldBe CharElement('Y')
    pst2.charAt(1) shouldBe CharElement('o')
    pst2.charAt(2) shouldBe ParameterPart(ps(5))
    pst2.charAt(3) shouldBe ParameterPart(ps(6))
    pst2.charAt(4) shouldBe CharElement(' ')
    pst2.charAt(23) shouldBe CharElement('!')
  }

}
