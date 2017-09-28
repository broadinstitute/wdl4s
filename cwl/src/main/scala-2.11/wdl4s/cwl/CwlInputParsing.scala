package wdl4s.cwl

import cats.syntax.validated._
import cats.syntax.either._
import io.circe._
import io.circe.shapes._
import io.circe.generic.auto._
import eu.timepit.refined.string._
import io.circe.refined._
import io.circe.yaml
import io.circe.literal._
import lenthall.validation.ErrorOr.ErrorOr
import lenthall.validation.Validation._
import lenthall.validation.Checked._
import shapeless.{:+:, CNil, Poly1}
import wdl4s.wdl.types.{WdlArrayType, WdlFileType, WdlType}
import wdl4s.wdl.values.{WdlArray, WdlValue}
import wdl4s.wom.executable.Executable.{InputParsingFunction, ParsedInputMap}

object CwlInputParsing {
  implicit val fileD = implicitly[Decoder[File]]
  implicit val stringD = implicitly[Decoder[String]]
  implicit val intD = implicitly[Decoder[Int]]
  implicit val longD = implicitly[Decoder[Long]]
  implicit val floatD = implicitly[Decoder[Float]]
  implicit val doubleD = implicitly[Decoder[Double]]
  implicit val booleanD = implicitly[Decoder[Boolean]]
  implicit val inputArray = implicitly[Decoder[Array[
    String :+:
      Int :+:
      Long :+:
      File :+:
      Float :+:
      Double :+:
      Boolean :+:
      CNil
    ]]]

  // Decodes the input file, and build the ParsedInputMap
  private [cwl] lazy val inputCoercionFunction: InputParsingFunction = inputFile => {
    yaml.parser.parse(inputFile).flatMap(_.as[Map[String, MyriadInputValue]]) match {
      case Left(error) => error.getMessage.invalidNelCheck[ParsedInputMap]
      case Right(inputValue) => inputValue.mapValues(_.fold(CwlInputCoercion)).validNelCheck
    }
  }
}

private object CwlInputCoercion extends Poly1 {
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

  implicit def inputArrayValueToWdlValue: Case.Aux[Array[
    String :+:
      Int :+:
      Long :+:
      File :+:
      Float :+:
      Double :+:
      Boolean :+:
      CNil
    ], WdlType => ErrorOr[WdlValue]] = at[Array[
    String :+:
      Int :+:
      Long :+:
      File :+:
      Float :+:
      Double :+:
      Boolean :+:
      CNil
    ]] { arrayValue =>
    womType: WdlType => {
      import cats.syntax.traverse._
      import cats.instances.list._
      
      womType match {
        case wdlArrayType: WdlArrayType =>
          arrayValue.toList
            .map(_.fold(CwlInputCoercion).apply(wdlArrayType.memberType))
            .sequence[ErrorOr, WdlValue]
            .map { WdlArray(wdlArrayType, _) }

        case other => s"Cannot convert an array input value into a non array type: $other".invalidNel
      }
    }
  }
}
