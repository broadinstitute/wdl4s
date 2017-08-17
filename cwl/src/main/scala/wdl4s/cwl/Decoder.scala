package wdl4s.cwl

import cats.data.{EitherT, NonEmptyList}
import ammonite.ops._
import cats.effect.IO
import shapeless.{Path => SPath, _}

object Decoder {

  type Parse[A] = EitherT[IO, NonEmptyList[String], A]

  def salad(path: Path): Parse[String] = ???

  def parseJson(json: String): Parse[Cwl] = ???

  def lookupNamesIn(in: Cwl): Parse[Map[String, Cwl]] = ???

  def modifyCwlWithEmbeddedCwl(in: Cwl, map: Map[String, Cwl]): Cwl =
    in.fold(AddEmbeddedCwl).apply(map)

  def decodeAllCwl(filename: Path): Parse[Cwl] =
    for {
      json <- salad(filename)
      unmodifiedCwl <- parseJson(json)
      modifyCwlWithEmbeddedCwl <- modifyCwlWithEmbeddedCwl(unmodifiedCwl)
    } yield modifyCwlWithEmbeddedCwl

}

