package wdl4s.types

import spray.json.JsObject
import wdl4s.values.{WdlPair, WdlValue}

import scala.language.postfixOps

case class WdlPairType(leftType: WdlType, rightType: WdlType) extends WdlType {

  override def isCoerceableFrom(otherType: WdlType): Boolean = otherType match {
    case WdlPairType(otherType1, otherType2) => leftType.isCoerceableFrom(otherType1) && rightType.isCoerceableFrom(otherType2)
    case jsObject: JsObject if jsObject.fields.size.equals(1) => rightType.coerceRawValue(jsObject.fields.head._2).isSuccess
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
    case jsObject: JsObject if jsObject.fields.size.equals(1) => jsObject.fields.head match {
      case (k, v) => WdlPair(leftType.coerceRawValue(k).get, rightType.coerceRawValue(v).get)
    }
  }

  override def toWdlString: String = s"Pair[${leftType.toWdlString}, ${rightType.toWdlString}]"
}
