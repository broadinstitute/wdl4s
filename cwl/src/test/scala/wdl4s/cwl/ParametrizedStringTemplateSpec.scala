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

  it should "do charAt, indexOf" in {
    Seq(1, 2, 6, 7, 25).map(pst1.charAt) shouldBe Seq('H', 'e', ' ', 'W', '!').map(CharElement)
    Seq(0, 13, 14, 26).map(pst1.charAt) shouldBe (0 to 3).map(i => ParameterPart(ps(i)))
    Seq(0, 1, 4, 23).map(pst2.charAt) shouldBe Seq('Y', 'o', ' ', '!').map(CharElement)
    Seq(2, 3).map(pst2.charAt) shouldBe Seq(5, 6).map(i => ParameterPart(ps(i)))
    Seq(0, 1, 2, 100).map(pst1.indexOf('H', _)) shouldBe Seq(1, 1, -1, -1)
    Seq(0, 20, 24, 25, 26).map(pst1.indexOf('y', _)) shouldBe Seq(24, 24, 24, -1, -1)
    Seq(0, 9, 10, 18, 19, 20, 100).map(pst2.indexOf('g', _)) shouldBe Seq(9, 9, 18, 18, -1, -1, -1)
  }

}
