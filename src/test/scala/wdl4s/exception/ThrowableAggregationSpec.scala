package wdl4s.exception

import org.scalatest.{FlatSpec, Matchers}

class ThrowableAggregationSpec extends FlatSpec with Matchers {

  "ThrowableAggregationSpec" should "aggregate errors in getMessage method" in {
    val exception = new RuntimeException with MessageAggregation {
      val exceptionContext = "This is absolutely NOT working."
      val errorMessages = List("because of A", "and also B", "and maybe C")
    }

    exception.getMessage shouldBe
      """This is absolutely NOT working.
        |because of A
        |and also B
        |and maybe C""".stripMargin
  }
}
