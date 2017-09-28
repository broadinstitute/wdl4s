package cwl

import cats.syntax.validated._
import lenthall.validation.ErrorOr.ErrorOr
import lenthall.validation.Validation._
import shapeless.Poly1
import wdl.types.{WdlArrayType, WdlFileType, WdlType}
import wdl.values.{WdlArray, WdlValue}

private [cwl] object CwlInputCoercion extends Poly1 {
  implicit def cwlFileToWdlValue: Case.Aux[MyriadInputValuePrimitives, WdlType => ErrorOr[WdlValue]] = at[MyriadInputValuePrimitives] {
    _.fold(CwlInputPrimitiveCoercion)
  }
  
  implicit def inputArrayValueToWdlValue: Case.Aux[Array[MyriadInputValuePrimitives], WdlType => ErrorOr[WdlValue]] =
    at[Array[MyriadInputValuePrimitives]] { arrayValue =>
      womType: WdlType => {
        import cats.instances.list._
        import cats.syntax.traverse._

        womType match {
          case wdlArrayType: WdlArrayType =>
            arrayValue.toList
              .map(_.fold(CwlInputPrimitiveCoercion).apply(wdlArrayType.memberType))
              .sequence[ErrorOr, WdlValue]
              .map { WdlArray(wdlArrayType, _) }

          case other => s"Cannot convert an array input value into a non array type: $other".invalidNel
        }
      }
    }
}

private [cwl] object CwlInputPrimitiveCoercion extends Poly1 {
  implicit def cwlFileToWdlValue: Case.Aux[File, WdlType => ErrorOr[WdlValue]] = at[File] { cwlFile =>
    womType: WdlType => {
      womType match {
        case WdlFileType => cwlFile.asWdlValue
        case otherType => s"Input value is a File but the targeted input is a $otherType".invalidNel
      }
    }
  }

  implicit def stringToWdlValue: Case.Aux[String, WdlType => ErrorOr[WdlValue]] = at[String] { stringValue =>
    womType: WdlType => {
      womType.coerceRawValue(stringValue).toErrorOr
    }
  }

  implicit def booleanToWdlValue: Case.Aux[Boolean, WdlType => ErrorOr[WdlValue]] = at[Boolean] { booleanValue =>
    womType: WdlType => {
      womType.coerceRawValue(booleanValue).toErrorOr
    }
  }

  implicit def intToWdlValue: Case.Aux[Int, WdlType => ErrorOr[WdlValue]] = at[Int] { intValue =>
    womType: WdlType => {
      womType.coerceRawValue(intValue).toErrorOr
    }
  }

  implicit def floatToWdlValue: Case.Aux[Float, WdlType => ErrorOr[WdlValue]] = at[Float] { floatValue =>
    womType: WdlType => {
      womType.coerceRawValue(floatValue).toErrorOr
    }
  }

  implicit def doubleToWdlValue: Case.Aux[Double, WdlType => ErrorOr[WdlValue]] = at[Double] { doubleValue =>
    womType: WdlType => {
      womType.coerceRawValue(doubleValue).toErrorOr
    }
  }

  implicit def longToWdlValue: Case.Aux[Long, WdlType => ErrorOr[WdlValue]] = at[Long] { longValue =>
    womType: WdlType => {
      womType.coerceRawValue(longValue).toErrorOr
    }
  }
}
