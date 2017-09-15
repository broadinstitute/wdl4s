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

  it should "do charAt(pos), indexOf(ch, fromIndex), indexOf(str, fromIndex)" in {
    Seq((pst1, -1), (pst1, 27), (pst1, 28), (pst2, -1), (pst2, 24), (pst2, 25)).foreach {
      case (pst, index) => assertThrows[IndexOutOfBoundsException](pst.charAt(index))
    }
    Seq(1, 2, 6, 7, 25).map(pst1.charAt) shouldBe Seq('H', 'e', ' ', 'W', '!').map(CharElement)
    Seq(0, 13, 14, 26).map(pst1.charAt) shouldBe (0 to 3).map(i => ParameterPart(ps(i)))
    Seq(0, 1, 4, 23).map(pst2.charAt) shouldBe Seq('Y', 'o', ' ', '!').map(CharElement)
    Seq(2, 3).map(pst2.charAt) shouldBe Seq(5, 6).map(i => ParameterPart(ps(i)))
    Seq(0, 1, 2, 100).map(pst1.indexOfChar('H', _)) shouldBe Seq(1, 1, -1, -1)
    Seq(0, 20, 24, 25, 26).map(pst1.indexOfChar('y', _)) shouldBe Seq(24, 24, 24, -1, -1)
    Seq(0, 9, 10, 18, 19, 20, 100).map(pst2.indexOfChar('g', _)) shouldBe Seq(9, 9, 18, 18, -1, -1, -1)
    Seq(0, 1, 2, 100).map(pst1.indexOfStr("Hello", _)) shouldBe Seq(1, 1, -1, -1)
    Seq(0, 20, 21, 22, 23, 24, 25).map(pst1.indexOfStr("day", _)) shouldBe Seq(22, 22, 22, 22, -1, -1, -1)
    Seq(0, 9, 10, 18, 19, 20, 100).map(pst2.indexOfStr("g", _)) shouldBe Seq(9, 9, 18, 18, -1, -1, -1)
  }

  it should "do toStringOption" in {
    pst1.toStringOption shouldBe None
    pst2.toStringOption shouldBe None
    val pst3 = ParametrizedStringTemplate.fromString("Hello World")
    pst3.toStringOption shouldBe Some("Hello World")
    val pst4 = pst3 + ", how are you?"
    pst4.toStringOption shouldBe Some("Hello World, how are you?")
  }

  it should "do substring" in {
    pst1.substring(0, 12) shouldBe ParametrizedStringTemplate.fromParameter(ps(0)) + "Hello World"
    pst1.substring(1, 12) shouldBe ParametrizedStringTemplate.fromString("Hello World")
    pst1.substring(7) shouldBe
      ParametrizedStringTemplate.fromString("World ") + ps(1) + ps(2) + "what a day!" + ps(3)
    pst1.substring(7, 19) shouldBe ParametrizedStringTemplate.fromString("World ") + ps(1) + ps(2) + "what"
    pst1.substring(26) shouldBe ParametrizedStringTemplate.fromParameter(ps(3))
  }

  it should "do hasParameters, nParameters, parameters, isBareParameter, isPrefixedParameter, prefix" in {
    val pure = ParametrizedStringTemplate.fromString("string")
    val bare = ParametrizedStringTemplate.fromParameter(ps(7))
    val prefixed = ParametrizedStringTemplate.fromString("prefix") + ps(7)
    val postfixed = ParametrizedStringTemplate.fromParameter(ps(7)) + "postfix"
    val sandwiched = ParametrizedStringTemplate.fromString("prefix") + ps(7) + "postfix"
    val examples = Seq(pst1, pst2, pure, bare, prefixed, postfixed, sandwiched)
    examples.map(_.hasParameters) shouldBe Seq(true, true, false, true, true, true, true)
    examples.map(_.nParameters) shouldBe Seq(4, 2, 0, 1, 1, 1, 1)
    examples.map(_.parameters) shouldBe
      Seq(Seq(ps(0), ps(1), ps(2), ps(3)), Seq(ps(5), ps(6)), Seq.empty, Seq(ps(7)), Seq(ps(7)),
        Seq(ps(7)), Seq(ps(7)))
    examples.map(_.isBareParameter) shouldBe Seq(false, false, false, true, false, false, false)
    examples.map(_.isPrefixedParameter) shouldBe Seq(false, false, false, false, true, false, false)
    examples.map(_.prefix) shouldBe Seq("", "Yo", "string", "", "prefix", "", "prefix")
  }

}
