package wdl4s.cwl

import shapeless.{Path => _, _}
import better.files.{File => BFile}
import cats.syntax.traverse._
import cats.syntax.either._
import cats.instances.list._
import cats.effect.IO
import CwlDecoder.{Parse, ParseValidated}
import cats.{Applicative, Monad}
import cats.data.EitherT._
import cats.data.{EitherT, NonEmptyList}
import lenthall.validation.ErrorOr._

object AddEmbeddedCwl extends Poly1 {

  implicit def workflow =
    at[Workflow] {
      workflow =>

        //Gather up all the filenames from the "run" section of workflow steps.
        val fileNames =
          workflow.
            steps.
            toList.
            flatMap(_.run.select[String].toList)

        //read the files, parse them, and put them in a Map
        val cwlMap: IO[Either[NonEmptyList[String], Map[String, Cwl]]] =
          fileNames.
            traverse[ParseValidated, (String, Cwl)] {
              path =>
                CwlDecoder.
                  decodeAllCwl(BFile(path)).
                  map(path -> _).value.map(_.toValidated)
            }(Applicative[IO] compose Applicative[ErrorOr]).

          map(_.map(_.toMap)).
          map(_.toEither)

        EitherT {
          cwlMap
        }.map{
          fileNameToCwl =>

            lens[Workflow].steps.modify(workflow){
              _.map{ step =>
                lens[WorkflowStep].run.modify(step)(_.fold(RunToEmbeddedCwl).apply(fileNameToCwl))
              }
            }.asCwl
        }
    }

  implicit def commandLineTool =
    at[CommandLineTool] {
      clt =>
        Monad[Parse].pure(clt.asCwl)
    }
}
