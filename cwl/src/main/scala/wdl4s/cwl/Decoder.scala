package wdl4s.cwl

import cats.data.{EitherT, NonEmptyList}
import ammonite.ops._
import cats.effect.IO
import shapeless.{Path => SPath, _}

object CwlDecoder {

  type Parse[A] = EitherT[IO, NonEmptyList[String], A]

  def salad(path: Path): Parse[String] = ???

  def parseJson(json: String): Parse[Cwl] = ???

  def lookupNamesIn(in: Cwl): Parse[Map[String, Cwl]] = ???

  def traverseFileNames(in: String): StateT[IO, Cwl,Unit] = StateT{ s => IO P


  def embedCwl(in: Workflow, map: Map[String, Cwl]): StateT[IO, Cwl, Unit] = {
    //take thing that goes from
    val filenames = in.steps.map(_.run.fold(


      in.steps.run.select[String].toList.traverse(traverseFileNames)
    }

  def modifyCwlWithEmbeddedCwl(in: Cwl, map: Map[String, Cwl]): Cwl =
    in.fold(AddEmbeddedCwl).apply(map)

  /**
   * Notice it gives you one instance of Cwl.  This has transformed all embedded files into scala object state
   */
  def decodeAllCwl(filename: Path): Parse[Cwl] =
    for {
      json <- salad(filename)//produces
      unmodifiedCwl <- parseJson(json)
      fileMap <- lookupNamesIn(unmodifiedCwl)
      cwlWithEmbeddedCwl = modifyCwlWithEmbeddedCwl(unmodifiedCwl, fileMap)
    } yield cwlWithEmbeddedCwl

}

