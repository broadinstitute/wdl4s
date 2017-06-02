package wdl4s.types

import lenthall.util.TryUtil
import spray.json.{JsObject, JsValue}
import wdl4s.values.{WdlPair, WdlValue}

import scala.language.postfixOps
import scala.util.{Failure, Try}

case class WdlPairType(leftType: WdlType, rightType: WdlType) extends WdlType {

  override def isCoerceableFrom(otherType: WdlType): Boolean = otherType match {
    case WdlPairType(otherType1, otherType2) => leftType.isCoerceableFrom(otherType1) && rightType.isCoerceableFrom(otherType2)
    case _ => false
  }

  /**
    * Method to be overridden by implementation classes defining a partial function
    * for the conversion of raw input values to specific implementation class value types.
    * i.e.  `WdlBooleanType` should define a partial function that knows how to
    * construct `WdlBoolean`s for inputs of supported types and contents.  Values for which
    * the partial function is not defined are assumed to not be convertible to the target type.
    */
  override protected def coercion: PartialFunction[Any, WdlValue] = {
    case otherPair @ WdlPair(otherValue1, otherValue2) if isCoerceableFrom(otherPair.wdlType) =>
      WdlPair(leftType.coerceRawValue(otherValue1).get, rightType.coerceRawValue(otherValue2).get)
    case jsObject: JsObject if jsObject.fields.size == 2 => mapToPair(jsObject.fields, this)
  }

  def mapToPair(m: Map[String, JsValue], wdlPairType: WdlPairType): WdlPair = {

    val caseNormalizedMap = m map { case(k, v) => k.toLowerCase.capitalize -> v }

    val wdlPair: Seq[Try[WdlValue]] = (caseNormalizedMap.get("Left"), caseNormalizedMap.get("Right")) match {
      case (Some(leftVal), Some(rightVal)) => Seq(wdlPairType.leftType.coerceRawValue(leftVal), wdlPairType.rightType.coerceRawValue(rightVal))
    }

    val failures = wdlPair collect { case f:Failure[_] => f }

    failures match {
      case f: Iterable[Failure[_]] if f.nonEmpty =>
        throw new UnsupportedOperationException(s"Failed to coerce one or more values for creating a ${wdlPairType.toWdlString}:\n${TryUtil.stringifyFailures(f)}}")
      case _ =>
        val coercedPair = wdlPair match { case Seq(left, right) => (left.get, right.get) }
        val leftType = WdlType.homogeneousTypeFromValues(Seq(coercedPair._1))
        val rightType = WdlType.homogeneousTypeFromValues(Seq(coercedPair._2))
        WdlPair(coercedPair._1, coercedPair._2)
    }
  }

  override def toWdlString: String = s"Pair[${leftType.toWdlString}, ${rightType.toWdlString}]"
}
