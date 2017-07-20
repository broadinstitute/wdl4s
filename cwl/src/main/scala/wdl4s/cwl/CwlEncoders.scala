package wdl4s.cwl

import io.circe.{Encoder, Json}
import wdl4s.cwl.CwlType.CwlType
import wdl4s.cwl.CwlVersion.CwlVersion

object CwlEncoders {
  implicit val cwlVersionEncoder: Encoder[CwlVersion] =
    (cwlVersion: CwlVersion) => Json.fromString(cwlVersion.toString)
  implicit val ecmaScriptExpressionEncoder: Encoder[ECMAScriptExpression] =
    (expression: ECMAScriptExpression) => Json.fromString(expression.toString)
  implicit val cwlTypeEncoder: Encoder[CwlType] =
    (cwlType: CwlType) => Json.fromString(cwlType.toString)
}
