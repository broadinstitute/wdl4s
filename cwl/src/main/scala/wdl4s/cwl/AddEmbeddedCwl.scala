package wdl4s.cwl

import shapeless._
import ammonite.ops._
import cats.syntax.traverse._
import cats.instances.list._
import cats.effect.IO
import CwlDecoder.Parse
import cats.Monad
import cats.data.EitherT._
import cats.data.EitherT
import cats.data.Validated

object AddEmbeddedCwl extends Poly1 {
  val workflowStepLens = lens[Workflow].steps
  val workflowStepRunLens = lens[WorkflowStep].run

  implicit def workflow =
    at[Workflow] {
      wf =>
        (relPath: RelPath) =>
          val cwlMap:Parse[Map[String, Cwl]] =
            wf.steps.toList.
              flatMap(_.run.select[String].toList).
              traverse[Parse, (String, Cwl)](
                path =>
                  CwlDecoder.
                    decodeAllCwl(pwd/relPath/path).
                    map(path -> _)).
              map(_.toMap)

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
