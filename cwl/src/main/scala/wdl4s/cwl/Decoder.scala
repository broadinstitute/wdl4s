package wdl4s.cwl

import cats.data.{EitherT, NonEmptyList}
import ammonite.ops._
import cats.effect.IO
import shapeless.{Path => SPath, _}

object CwlDecoder {

  type Parse[A] = EitherT[IO, NonEmptyList[String], A]

  def salad(path: Path): Parse[String] = ???

  def parseJson(json: String): Parse[Cwl] = ???

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

