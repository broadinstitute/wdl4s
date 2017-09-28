package wdl

import cats.syntax.either._
import scala.util.Try

private [wdl] object WdlInputParsing {

  private [wdl] lazy val inputCoercionFunction: InputParsingFunction = inputString => {
    import lenthall.validation.Checked._
    import lenthall.validation.Validation._
    import spray.json._

    Try(inputString.parseJson).toErrorOr.toEither flatMap {
      case JsObject(fields) => fields.mapValues(jsValue => { womType: WdlType =>
        womType.coerceRawValue(jsValue).toErrorOr
      }).validNelCheck
      case other => s"WDL input file must be a valid Json object. Found a ${other.getClass.getSimpleName}".invalidNelCheck[ParsedInputMap]
    }
  }
}

