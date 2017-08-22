package wdl4s.cwl

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import ammonite.ops._
import cats.data.NonEmptyList
import shapeless.Coproduct

class CwlDecoderSpec extends Properties("cwl decoder") {
    property("read nested workflow") = {
      val value = CwlDecoder.decodeAllCwl(pwd/'cwl/'src/'test/'resources/"nestedworkflows.cwl").value

      val result: Either[NonEmptyList[String], Cwl] = value.unsafeRunSync

      result match {
        case Right(cwl) =>
          val wf = cwl.select[Workflow].get
          wf.steps.flatMap(_.run.select[String].toList).size == 0
        case Left(other) => throw new RuntimeException(other.toList.mkString(", "))
      }
    }
}
