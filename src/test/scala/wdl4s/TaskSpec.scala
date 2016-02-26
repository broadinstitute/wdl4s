package wdl4s

import scala.util.Failure
import better.files._
import wdl4s.expression.NoFunctions
import wdl4s.types._
import wdl4s.values._

class TaskSpec extends WdlTest {
  val threeStepWdl = "src/test/cases/three_step/test.wdl"
  val commandParameterWdl = "src/test/cases/command_parameters/test.wdl"

  threeStepWdl should {
    val namespace = loadWdlFile(File(threeStepWdl))
    val wcTask = getTask(namespace, "wc")
    val cgrepTask = getTask(namespace, "cgrep")
    val psTask = getTask(namespace, "ps")

    s"have a task with name 'wc'" in {
      wcTask.name shouldEqual "wc"
      wcTask.declarations.map(_.toWdlString) shouldEqual Vector("File in_file")
      wcTask.instantiateCommand(Map("wc.in_file" -> WdlFile("/path/to/file")), NoFunctions).get shouldEqual "cat /path/to/file | wc -l"
      wcTask.outputs.size shouldEqual 1
      wcTask.outputs.head.unqualifiedName shouldEqual "count"
      wcTask.outputs.head.wdlType shouldEqual WdlIntegerType
    }

    s"have a task with name 'cgrep'" in {
      cgrepTask.name shouldEqual "cgrep"
      cgrepTask.declarations.map(_.toWdlString) shouldEqual Vector(
        "String pattern",
        "File in_file"
      )
      cgrepTask.instantiateCommand(Map("cgrep.pattern" -> WdlString("^...$"), "cgrep.in_file" -> WdlFile("/path/to/file")),
        NoFunctions).get shouldEqual "grep '^...$' /path/to/file | wc -l"
      cgrepTask.outputs.size shouldEqual 1
      cgrepTask.outputs.head.unqualifiedName shouldEqual "count"
      cgrepTask.outputs.head.wdlType shouldEqual WdlIntegerType
    }

    s"have a task with name 'ps'" in {
      psTask.name shouldEqual "ps"
      psTask.declarations shouldEqual Vector()
      psTask.instantiateCommand(Map(), NoFunctions).get shouldEqual "ps"
      psTask.outputs.size shouldEqual 1
      psTask.outputs.head.unqualifiedName shouldEqual "procs"
      psTask.outputs.head.wdlType shouldEqual WdlFileType
    }
  }

  commandParameterWdl should {
    val namespace = loadWdlFile(File(commandParameterWdl))
    val paramTestTask = getTask(namespace, "param_test")

    s"instantiate command (0)" in {
      val inputs = Map(
        "param_test.a" -> WdlString("a_val"),
        "param_test.b" -> WdlString("b_val"),
        "param_test.c" -> WdlArray(WdlArrayType(WdlStringType), Seq(WdlString("c0"), WdlString("c1"), WdlString("c2"))),
        "param_test.d" -> WdlInteger(1),
        "param_test.e" -> WdlArray(WdlArrayType(WdlIntegerType), Seq(0, 1, 2).map(WdlInteger(_))),
        "param_test.f" -> WdlBoolean.False
      )
      paramTestTask.instantiateCommand(inputs, NoFunctions).get shouldEqual "./binary a_val -p b_val c0,c1,c2 1 0\t1\t2 --false"
    }

    s"instantiate command (1)" in {
      val inputs = Map(
        "param_test.a" -> WdlString("a_val"),
        "param_test.b" -> WdlString("b_val"),
        "param_test.c" -> WdlArray(WdlArrayType(WdlStringType), Seq(WdlString("c0"), WdlString("c1"), WdlString("c2"))),
        "param_test.e" -> WdlArray(WdlArrayType(WdlIntegerType), Seq(0, 1, 2).map(WdlInteger(_))),
        "param_test.f" -> WdlBoolean.True
      )
      paramTestTask.instantiateCommand(inputs, NoFunctions).get shouldEqual "./binary a_val -p b_val c0,c1,c2 9 0\t1\t2 --true"
    }

    s"instantiate command (2)" in {
      val inputs = Map(
        "param_test.a" -> WdlString("a_val"),
        "param_test.b" -> WdlString("b_val"),
        "param_test.c" -> WdlArray(WdlArrayType(WdlStringType), Seq(WdlString("c0"))),
        "param_test.d" -> WdlInteger(1),
        "param_test.e" -> WdlArray(WdlArrayType(WdlIntegerType), Seq()),
        "param_test.f" -> WdlBoolean.True
      )
      paramTestTask.instantiateCommand(inputs, NoFunctions).get shouldEqual "./binary a_val -p b_val c0 1  --true"
    }

    s"instantiate command (3)" in {
      val inputs = Map(
        "param_test.a" -> WdlString("a_val"),
        "param_test.b" -> WdlString("b_val"),
        "param_test.c" -> WdlArray(WdlArrayType(WdlStringType), Seq()),
        "param_test.d" -> WdlInteger(1),
        "param_test.e" -> WdlArray(WdlArrayType(WdlIntegerType), Seq()),
        "param_test.f" -> WdlBoolean.True
      )
      paramTestTask.instantiateCommand(inputs, NoFunctions).get shouldEqual "./binary a_val -p b_val  1  --true"
    }

    s"fail to instantiate command if missing a required input" in {
      paramTestTask.instantiateCommand(Map("param_test.a" -> WdlString("a_val")), NoFunctions) match {
        case Failure(f) => // expected
        case _ => fail("Expected an exception")
      }
    }

    s"fail to instantiate command if an input is an expression" in {
      val inputs = Map(
        "param_test.a" -> WdlString("a_val"),
        "param_test.b" -> WdlExpression.fromString("'a'+'b'"),
        "param_test.c" -> WdlArray(WdlArrayType(WdlStringType), Seq()),
        "param_test.d" -> WdlInteger(1),
        "param_test.e" -> WdlArray(WdlArrayType(WdlIntegerType), Seq())
      )
      paramTestTask.instantiateCommand(inputs, NoFunctions) match {
        case Failure(f) => // expected
        case _ => fail("Expected an exception")
      }
    }

  }
}
