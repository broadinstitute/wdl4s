package wdl4s.cwl

import eu.timepit.refined.api.Refined
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}
import wdl4s.cwl.CwlType.CwlType
import wdl4s.cwl.CwlVersion.CwlVersion

/**
  * wdl4s
  * Created by oruebenacker on 06.07.17.
  */
object CwlEncoders {
  implicit val cwlVersionEncoder: Encoder[CwlVersion] =
    (cwlVersion: CwlVersion) => Json.fromString(cwlVersion.toString)
  implicit val ecmaScriptExpressionEncoder: Encoder[ECMAScriptExpression] =
    (a: ECMAScriptExpression) => Json.fromString(a.toString)
  implicit val cwlTypeEncoder: Encoder[CwlType] =
    (cwlType: CwlType) => Json.fromString(cwlType.toString)
  implicit def refinedEncoder[V,P](implicit valueEncoder: Encoder[V]): Encoder[Refined[V, P]] =
    (refined: Refined[V, P]) => refined.value.asJson
}
