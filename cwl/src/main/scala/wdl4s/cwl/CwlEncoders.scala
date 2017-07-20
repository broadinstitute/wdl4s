package wdl4s.cwl

import eu.timepit.refined.api.Refined
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}
import shapeless.Witness
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
    (expression: ECMAScriptExpression) => Json.fromString(expression.toString)
  implicit val cwlTypeEncoder: Encoder[CwlType] =
    (cwlType: CwlType) => Json.fromString(cwlType.toString)
}
