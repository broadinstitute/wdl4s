package wdl4s

import wdl4s.parser.WdlParser.SyntaxError

import scala.util.{Failure, Success}
import org.scalatest.{EitherValues, Matchers, FlatSpec}
import RuntimeAttributeSpec._
import wdl4s.expression.NoFunctions
import wdl4s.util.AggregatedException
import wdl4s.values.{WdlString, WdlInteger}

object RuntimeAttributeSpec {
  val WorkflowWithRuntime =
    """
      |task ps {
      |  command {
      |    ps
      |  }
      |  output {
      |    File procs = stdout()
      |  }
      |  runtime {
      |    docker: "ubuntu:latest"
      |  }
      |}
      |
      |task cgrep {
      |  String pattern
      |  File in_file
      |  command {
      |    grep '${pattern}' ${in_file} | wc -l
      |  }
      |  output {
      |    Int count = read_int(stdout())
      |  }
      |  runtime {
      |    docker: "ubuntu:latest"
      |  }
      |}
      |
      |task wc {
      |  File in_file
      |  command {
      |    cat ${in_file} | wc -l
      |  }
      |  output {
      |    Int count = read_int(stdout())
      |  }
      |  runtime {
      |     docker: "ubuntu:latest"
      |  }
      |}
      |
      |workflow three_step {
      |  call ps
      |  call cgrep {
      |    input: in_file=ps.procs
      |  }
      |  call wc {
      |    input: in_file=ps.procs
      |  }
      |}
      |
    """.stripMargin

  val WorkflowWithoutRuntime =
    """
      |task hello {
      |  String addressee
      |  command {
      |    echo "Hello ${addressee}!"
      |  }
      |  output {
      |    String salutation = read_string(stdout())
      |  }
      |}
      |
      |workflow hello_workflow {
      |  call hello
      |}
    """.stripMargin

  val WorkflowWithMessedUpMemory =
    """
    |task messed_up_memory {
    |  command {
    |      echo "YO"
    |  }
    |  runtime {
    |    memory: "HI TY"
    |  }
    |}
    |
    |workflow great_googly_moogly {
    |  call messed_up_memory
    |}
  """.stripMargin
  val WorkflowWithMessedUpMemoryUnit =
    """
      |task messed_up_memory {
      |  command {
      |      echo "YO"
      |  }
      |  runtime {
      |    memory: "5 TY"
      |  }
      |}
      |
      |workflow great_googly_moogly {
      |  call messed_up_memory
      |}
    """.stripMargin
  val WorkflowWithMemoryExpression =
    """
      |task memory_expression {
      |  Int gb
      |  command {
      |      echo "YO"
      |  }
      |  runtime {
      |    memory: gb + " GB"
      |  }
      |}
      |
      |workflow great_googly_moogly {
      |  call memory_expression
      |}
    """.stripMargin
  val WorkflowWithMemoryExpressionAndStringInterpolation =
    """
      |task memory_expression {
      |  Int gb
      |  command {
      |      echo "YO"
      |  }
      |  runtime {
      |    memory: "${gb} GB"
      |  }
      |}
      |
      |workflow great_googly_moogly {
      |  call memory_expression
      |}
    """.stripMargin
  }

class RuntimeAttributeSpec extends FlatSpec with Matchers with EitherValues {
  val NamespaceWithRuntime = WdlNamespaceWithWorkflow.load(WorkflowWithRuntime)
  val NamespaceWithoutRuntime = WdlNamespaceWithWorkflow.load(WorkflowWithoutRuntime)

  "WDL file with runtime attributes" should "have attribute maps" in {
    NamespaceWithRuntime.tasks.forall(_.runtimeAttributes.attrs.nonEmpty) should be(true)
  }

  "WDL file without runtime attributes" should "not have attribute maps" in {
    NamespaceWithoutRuntime.tasks.forall(_.runtimeAttributes.attrs.isEmpty) should be(true)
  }

  "WDL file with a 'memory' runtime attribute" should "throw an exception if a malformed static string is specified" in {
    val ex = intercept[SyntaxError] {
      WdlNamespaceWithWorkflow.load(WorkflowWithMessedUpMemory)
    }

    ex.getMessage should include ("ERROR: 'memory' runtime attribute should have the format \"size unit\" (e.g. \"8 GB\")")
  }

  it should "throw an exception if the static string contains an invalid memory unit" in {
    val ex = intercept[SyntaxError] {
      WdlNamespaceWithWorkflow.load(WorkflowWithMessedUpMemoryUnit)
    }

    ex.getMessage should include ("ERROR: 'memory' runtime attribute should have the format \"size unit\" (e.g. \"8 GB\")")
  }

  it should "accept a value that contains an expression that can't be statically evaluated (1)" in {
    val ns = WdlNamespaceWithWorkflow.load(WorkflowWithMemoryExpressionAndStringInterpolation)
    val runtime = ns.findTask("memory_expression").get.runtimeAttributes.evaluate((s:String) => WdlInteger(10), NoFunctions)
    runtime("memory") shouldBe a[Success[_]]
    runtime("memory").get shouldEqual WdlString("10 GB")
  }

  it should "accept a value that contains an expression that can't be statically evaluated (2)" in {
    val ns = WdlNamespaceWithWorkflow.load(WorkflowWithMemoryExpression)
    val runtime = ns.findTask("memory_expression").get.runtimeAttributes

    val goodRuntime = runtime.evaluate((s:String) => WdlInteger(4), NoFunctions)
    goodRuntime("memory") shouldBe a[Success[_]]
    goodRuntime("memory").get shouldEqual WdlString("4 GB")

    val badRuntime = runtime.evaluate((s:String) => WdlString("foobar"), NoFunctions)
    badRuntime("memory") shouldBe a[Failure[_]]
  }
}
