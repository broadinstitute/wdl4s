package wdl4s.cwl

import cats.data.{EitherT, NonEmptyList, ValidatedNel}
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import cats.effect.IO
import cats.syntax.either._

import scala.util.Try

object CwlDecoder {

  type Parse[A] = EitherT[IO, NonEmptyList[String], A]
  type ParseValidated[A] = IO[ValidatedNel[String, A]]

  def salad(path: Path): Parse[String] = {
    def resultToEither(cr: CommandResult) =
      cr.exitCode match {
        case 0 => Right(cr.out.string)
        case error => Left(NonEmptyList.one(s"running CwlTool on file $path resulted in exit code $error and stderr ${cr.err.string}"))
      }

    val cwlToolResult =
      Try(%%("cwltool", "--quiet", "--print-pre", path.toString)).
        toEither.
        leftMap(t => NonEmptyList.one(s"running cwltool on file $path failed with ${t.getMessage}"))

    EitherT {
      IO {
        for {
          _cwlToolResult <- cwlToolResult
          result <- resultToEither(_cwlToolResult)
        } yield result
      }
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
      cwlWithEmbeddedCwl <- unmodifiedCwl.fold(AddEmbeddedCwl)
    } yield cwlWithEmbeddedCwl

}

