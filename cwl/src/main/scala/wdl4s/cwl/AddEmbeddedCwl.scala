package wdl4s.cwl

import shapeless.{Path => SPath, _}
import ammonite.ops._
import cats.syntax.traverse._
import cats.syntax.either._
import cats.instances.list._
import cats.effect.IO
import CwlDecoder.{Parse, ParseValidated}
import cats.{Applicative, Monad}
import cats.data.EitherT._
import cats.data.{EitherT, Validated, ValidatedNel}
import lenthall.validation.ErrorOr.ErrorOr

object AddEmbeddedCwl extends Poly1 {
  val workflowStepLens = lens[Workflow].steps
  val workflowStepRunLens = lens[WorkflowStep].run

  implicit def workflow =
    at[Workflow] {
      wf =>
        (relPath: RelPath) =>
          println(s"relPath was $relPath")
          val filenames = wf.steps.toList.
            flatMap(_.run.select[String].toList)
          println(s"filenames: $filenames")

          def consumePath(path: Path): ParseValidated[(String, Cwl)] = {
              CwlDecoder.
                decodeAllCwl(path).
                map(path -> _).value.map(_.toValidated)
          }

          def cwlMapValidated(ammonitePath: Path): Parse[Map[String, Cwl]] = EitherT {
        wf.steps.toList.
          flatMap(_.run.select[String].toList).
          traverse(fileName => CwlDecoder.constructPath(fileName).map(fileName -> )).
          traverse[ParseValidated, (String, Cwl)] {
          path =>
            CwlDecoder.
              decodeAllCwl(path).
              map(path -> _).value.map(_.toValidated)
        }(Applicative[IO] compose Applicative[ErrorOr]).
          map(_.map(_.toMap)).map(_.toEither)
      }

          /*
          val cwlMap: Parse[Map[String, Cwl]] =
            for {
              ammPath <- CwlDecoder.constructPath(rel)
              EitherT{  cwlMapValidated.map(_.toEither) }

              cwlMap.map{
                map =>

                  workflowStepLens.modify(wf){
                    _.map{ step =>
                      workflowStepRunLens.modify(step)(_.fold(RunToEmbeddedCwl).apply(map))
                    }
                  }.asCwl
              }
            }
            */
        ???
    }

  implicit def commandLineTool =
    at[CommandLineTool] {
      clt =>
        (_: RelPath) =>
          Monad[Parse].pure(clt.asCwl)
    }
}
