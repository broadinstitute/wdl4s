package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}

class ParametrizedStringTemplateSpec extends FlatSpec with Matchers {

  case class MockParameter(id: Int)

  def para(i: Int): MockParameter = MockParameter(i)

  it should "properly construct parametrized string templates" in {
    val pst1 =
      ParametrizedStringTemplate.fromParameter[MockParameter](para(0)) + "Hello " + "World " +
        para(1) + para(2) + "what a day!" + para(3)
    val pst2 =
      ParametrizedStringTemplate.fromString[MockParameter]("Yo") + para(5) + para(6) +
        " dis" + " gonna" + " be" + " great!"
    pst1.parts.size shouldBe 6
    pst2.parts.size shouldBe 4
    val paraToString = (para: MockParameter) => para.id.toString
    pst1.toString(paraToString) shouldBe "0Hello World 12what a day!3"
    pst2.toString(paraToString) shouldBe "Yo56 dis gonna be great!"
    pst1.lengthSums shouldBe Seq(1, 13, 14, 15, 26, 27)
    pst2.lengthSums shouldBe Seq(2, 3, 4, 24)

  }

}
