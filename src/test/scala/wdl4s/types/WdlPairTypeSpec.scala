package wdl4s.types

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}
import spray.json.{JsArray, JsNumber, JsObject}
import wdl4s.WdlNamespaceWithWorkflow
import wdl4s.values.{WdlArray, WdlInteger, WdlMap, WdlPair, WdlString}

import scala.util.{Failure, Success}

class WdlPairTypeSpec extends FlatSpec with Matchers {

  behavior of "WdlPairType"

  val stringIntMap = WdlMap(WdlMapType(WdlStringType, WdlIntegerType), Map(
    WdlString("a") -> WdlInteger(1),
    WdlString("b") -> WdlInteger(2),
    WdlString("c") -> WdlInteger(3)
  ))

  val arrayOfPairs = WdlArray(WdlArrayType(WdlPairType(WdlStringType, WdlIntegerType)), Seq(
    WdlPair(WdlString("a"),WdlInteger(1)),
    WdlPair(WdlString("b"),WdlInteger(2)),
    WdlPair(WdlString("c"),WdlInteger(3))
  ))

  val arrayOfPairsOfArrays = WdlArray(WdlArrayType(WdlPairType(WdlStringType, WdlArrayType(WdlIntegerType))), Seq(
    WdlPair(WdlString("a"), WdlArray(WdlArrayType(WdlIntegerType), Seq(WdlInteger(1), WdlInteger(11)))),
    WdlPair(WdlString("b"), WdlArray(WdlArrayType(WdlIntegerType), Seq(WdlInteger(2), WdlInteger(21)))),
    WdlPair(WdlString("c"), WdlArray(WdlArrayType(WdlIntegerType), Seq(WdlInteger(3), WdlInteger(31))))
  ))

  val coerceables = Table(
    ("fromValue", "toType", "coercedValue"),
    (
      WdlPair(WdlInteger(1), WdlInteger(2)),
      WdlPairType(WdlStringType, WdlStringType),
      Some(WdlPair(WdlString("1"), WdlString("2")))
    ),

    (
      WdlPair(WdlMap(WdlMapType(WdlIntegerType, WdlStringType), Map(
      WdlInteger(1) -> WdlString("100"),
      WdlInteger(2) -> WdlString("200")
      )), WdlString("300")),
      WdlPairType(WdlMapType(WdlStringType, WdlIntegerType), WdlIntegerType),
      Some(WdlPair(WdlMap(WdlMapType(WdlStringType, WdlIntegerType), Map(
        WdlString("1") -> WdlInteger(100),
        WdlString("2") -> WdlInteger(200)
      )), WdlInteger(300)))
    ),

    (
      WdlPair(WdlMap(WdlMapType(WdlIntegerType, WdlStringType), Map(
        WdlInteger(1) -> WdlString("100"),
        WdlInteger(2) -> WdlString("200")
      )), WdlString("300")),
      WdlPairType(WdlArrayType(WdlStringType), WdlStringType),
      None)

  )

  coerceables foreach { case (fromValue, toType, coercedValue) =>

    val notString = coercedValue map { _ => "" } getOrElse "not "
    val coercionDefined = coercedValue.isDefined

    it should s"${notString}allow coercion from ${fromValue.wdlType.toWdlString} to ${toType.toWdlString}" in {
      toType.isCoerceableFrom(fromValue.wdlType) should be(coercionDefined)
    }

    it should s"generate the expected result when converting from ${fromValue.toWdlString} to ${toType.toWdlString}" in {
      val actual = toType.coerceRawValue(fromValue)
      (actual, coercedValue) match {
        case (Success(actualValue), Some(expectedValue)) => actualValue should be(expectedValue)
        case (Success(actualValue), None) => fail(s"Coercion should have failed but instead got $actualValue")
        case (Failure(_), None) => // Correctly failed to coerce
        case (Failure(t), Some(expectedValue)) => fail(s"Expected coercion to produce $expectedValue but instead got exception $t")
      }
    }
  }

  it should "Allow use of pairs in call input mappings" in {
    val wdl =
      """
        |task t {
        |  File f1
        |  File f2
        |  File f3
        |  File f4
        |  File f5
        |  command {...}
        |  output {
        |    File o1 = f1
        |    File o2 = f2
        |    File o3 = f3
        |    File o4 = f4
        |    File o5 = f5
        |  }
        |}
        |
        |workflow w {
        |  Array[Pair[File, Array[File]]] arr = [("left_file", ["right_file1", "right_file2"])]
        |  Pair[String, String] p = ("hello", "world")
        |
        |  scatter (i in arr) {
        |    call t {
        |      input:
        |       f1 = i.left,
        |       f2 = i.right[0],
        |       f3 = i.right[1],
        |       f4 = p.left,
        |       f5 = p.right
        |    }
        |  }
        |}
      """.stripMargin

    noException should be thrownBy WdlNamespaceWithWorkflow.load(wdl, Seq.empty)
  }

  it should "coerce a Map into a WdlArray of WdlPair type" in {
    WdlArrayType(WdlPairType(WdlStringType, WdlIntegerType)).coerceRawValue(stringIntMap) match {
      case Success(array) => array shouldEqual arrayOfPairs
      case Failure(f) => fail(s"exception while coercing JsObject: $f")
    }
  }

  it should "coerce a JsArray into a WdlArray of WdlPairs" in {
    val jsArray = JsArray(JsObject("a" -> JsNumber(1)), JsObject("b" -> JsNumber(2)), JsObject("c" -> JsNumber(3)))

    WdlArrayType(WdlPairType(WdlStringType, WdlIntegerType)).coerceRawValue(jsArray) match {
      case Success(array) => array shouldEqual arrayOfPairs
      case Failure(f) => fail(s"exception while coercing JsObject: $f")
    }
  }

  it should "coerce a complex JsArray into a WdlArray of WdlPairs of WdlArrays" in {
    val complexJsArray =  JsArray(JsObject("a" -> JsArray(JsNumber(1), JsNumber(11))),
                                  JsObject("b" -> JsArray(JsNumber(2), JsNumber(21))),
                                  JsObject("c" -> JsArray(JsNumber(3), JsNumber(31))))

    WdlArrayType(WdlPairType(WdlStringType, WdlArrayType(WdlIntegerType))).coerceRawValue(complexJsArray) match {
      case Success(array) => array shouldEqual arrayOfPairsOfArrays
      case Failure(f) => fail(s"exception while coercing JsObject: $f")
    }
  }
}
