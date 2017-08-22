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
  property("broken links fail the SALAD preprocessing test") = {
    val value = CwlDecoder.decodeAllCwl(pwd/'cwl/'src/'test/'resources/"brokenlinks.cwl").value

    value.unsafeRunSync.isLeft
  }
  property("links fail to parse breaks the SALAD preprocessing test") = {
    val value = CwlDecoder.decodeAllCwl(pwd/'cwl/'src/'test/'resources/"links_dont_parse.cwl").value

    val result: Either[NonEmptyList[String], Cwl] = value.unsafeRunSync

    result match {
      case Right(_) =>throw new RuntimeException("should not have parsed linke cwl")
      case Left(x) =>  println(x); true
    }
  }
}
