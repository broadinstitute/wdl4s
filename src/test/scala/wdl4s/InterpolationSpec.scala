package wdl4s

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.expression.NoFunctions
import wdl4s.values._

import scala.util.Try

class InterpolationSpec extends FlatSpec with Matchers {
  val wdl =
    """
      |task test {
      |  String eval_this
      |  String var="inside"
      |
      |  # this should evaluate to "val"
      |  String evaled="${eval_this}"
      |
      |  command {
      |    echo '${eval_this} ${evaled} ${var}'
      |  }
      |
      |  output {
      |    # I expect the output to be "var inside inside"
      |    String out = read_string(stdout())
      |  }
      |}
      |
      |workflow testWF {
      |  String var = "outside"
      |  call test
      |  call test as test2 { input:
      |    eval_this="${var}"
      |  }
      |}
    """.stripMargin
  val namespace = NamespaceWithWorkflow.load(wdl)
  val test = namespace.workflow.calls find {_.unqualifiedName == "test"} getOrElse {
    fail("call 'test' not found")
  }
  val test2 = namespace.workflow.calls find {_.unqualifiedName == "test2"} getOrElse {
    fail("call 'test2' not found")
  }

  it should "foobar" in {
    val testCmd = test.task.instantiateCommand(Map("eval_this" -> WdlString("${var}")), NoFunctions)
    val test2Cmd = test2.task.instantiateCommand(Map(), NoFunctions)
    testCmd shouldEqual Try("echo 'inside inside inside'")
    test2Cmd shouldEqual Try("echo 'outside inside inside'")
  }
}
