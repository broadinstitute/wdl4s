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
import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import lenthall.validation.ErrorOr.ErrorOr

object AddEmbeddedCwl extends Poly1 {
  val workflowStepLens = lens[Workflow].steps
  val workflowStepRunLens = lens[WorkflowStep].run

  implicit def workflow =
    at[Workflow] {
      wf =>
        (relPath: RelPath) =>

          val cwlMap: Parse[Map[String, Cwl]] = EitherT {
            wf.steps.toList.
              flatMap(_.run.select[String].toList).
              traverse[ParseValidated, (String, Cwl)] {
              path =>
                CwlDecoder.
                  decodeAllCwl(Path(path.drop(5))).
                  map(path -> _).value.map(_.toValidated)
            }(Applicative[IO] compose Applicative[ErrorOr]).
              map(_.map(_.toMap)).map(_.toEither)
          }

              cwlMap.map{
                map =>

                  workflowStepLens.modify(wf){
                    _.map{ step =>
                      workflowStepRunLens.modify(step)(_.fold(RunToEmbeddedCwl).apply(map))
                    }
                  }.asCwl
              }
    }

  implicit def commandLineTool =
    at[CommandLineTool] {
      clt =>
        (_: RelPath) =>
          Monad[Parse].pure(clt.asCwl)
    }
}
