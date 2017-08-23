package wdl4s.cwl

import org.scalacheck.Properties
import org.scalacheck.Prop.BooleanOperators
import CwlDecoder._

class CwlDecoderSpec extends Properties("cwl decoder") {

  import TestSetup._

  property("read nested workflow") =
    decodeAllCwl(rootPath/"nestedworkflows.cwl").
      value.
      unsafeRunSync match {
        case Right(cwl) =>
          val wf = cwl.select[Workflow].get
          wf.steps.flatMap(_.run.select[String].toList).size == 0
        case Left(other) => false :| other.toList.mkString(", ")
      }

  property("broken links fail the SALAD preprocessing test") =
    decodeAllCwl(rootPath/"brokenlinks.cwl").
      value.
      unsafeRunSync.
      isLeft

  property("links fail to parse breaks the SALAD preprocessing test") =
    decodeAllCwl(rootPath/"links_dont_parse.cwl").
     value.
     unsafeRunSync.
     isLeft
}
