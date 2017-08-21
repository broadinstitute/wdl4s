package wdl4s.cwl

import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import cats.effect.IO
import shapeless.{Path => SPath, _}
import mouse.all._
import cats.syntax.either._

import scala.util.Try



object CwlDecoder {

  type Parse[A] = EitherT[IO, NonEmptyList[String], A]
  type ParseValidated[A] = IO[ValidatedNel[String, A]]

  def constructPath(relativePath: RelPath, path: String): ParseValidated[Path] =
    IO {
      Try(pwd/relativePath/path).toEither.toValidated.leftMap(_.getMessage).leftMap(NonEmptyList.one)
    }

  def salad(path: Path): Parse[String] = EitherT {
    IO {
      for {
        cwlTooLResult <- Try(%%("cwltool", "--quiet", "--print-pre", path.toString)).toEither.
          leftMap(t => NonEmptyList.one(s"running cwltool failed with ${t.getMessage}"))
        result <-
          cwlTooLResult.exitCode match {
            case 0 => Right(cwlTooLResult.out.string)
            case error => Left(NonEmptyList.one(s"CwlTool on Salad resulted in exit code $error and stderr ${cwlTooLResult.err.string}"))
          }
      } yield result
    }
  }

  def parseJson(json: String): Parse[Cwl] =
    EitherT{IO{CwlCodecs.decodeCwl(json).leftMap(_.getMessage).leftMap(NonEmptyList.one)} }

  /**
   * Notice it gives you one instance of Cwl.  This has transformed all embedded files into scala object state
   */
  def decodeAllCwl(fileName: Path): Parse[Cwl] =
    for {
      jsonString <- salad(fileName)
      unmodifiedCwl <- parseJson(jsonString)
      cwlWithEmbeddedCwl <- unmodifiedCwl.fold(AddEmbeddedCwl).apply(fileName relativeTo pwd)
    } yield cwlWithEmbeddedCwl

}

