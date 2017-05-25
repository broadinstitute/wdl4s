package wdl4s.values

import lenthall.util.TryUtil
import wdl4s.types.{WdlMapType, WdlPairType, WdlType}

import scala.util.Failure

case class WdlPair(left: WdlValue, right: WdlValue) extends WdlValue {
  override val wdlType = WdlPairType(left.wdlType, right.wdlType)

  def coercePair(pairs: Seq[(_,_)], wdlPairType: WdlPairType): Seq[WdlPair] = {
    val coerced = pairs map { case(k, v) => wdlPairType.leftType.coerceRawValue(k) -> wdlPairType.rightType.coerceRawValue(v) }
    val failures = coerced flatMap { case(k,v) => Seq(k,v) } collect { case f:Failure[_] => f }
    failures match {
      case f: Iterable[Failure[_]] if f.nonEmpty =>
        throw new UnsupportedOperationException(s"Failed to coerce one or more keys or values for creating a ${wdlPairType.toWdlString}:\n${TryUtil.stringifyFailures(f)}}")
      case _ =>
        val pairCoerced = coerced map { case (k, v) => k.get -> v.get }

        val leftType = WdlType.homogeneousTypeFromValues(pairCoerced map { case (k, v) => k })
        val rightType = WdlType.homogeneousTypeFromValues(pairCoerced map { case (k, v) => v })

        WdlMap(WdlMapType(leftType, rightType), pairCoerced)
    }
  }

  override def toWdlString = s"(${left.toWdlString}, ${right.toWdlString})"

  override def collectAsSeq[T <: WdlValue](filterFn: PartialFunction[WdlValue, T]): Seq[T] = {
    left.collectAsSeq(filterFn) ++ right.collectAsSeq(filterFn)
  }
}
