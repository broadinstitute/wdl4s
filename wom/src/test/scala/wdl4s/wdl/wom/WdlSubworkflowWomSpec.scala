package wdl4s.wdl.wom

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.types.{WdlArrayType, WdlIntegerType, WdlStringType}
import wdl4s.wdl.{ImportResolver, WdlNamespace, WdlNamespaceWithWorkflow}
import wdl4s.wom.graph.{Graph, TaskCallNode, WorkflowCallNode}

class WdlSubworkflowWomSpec extends FlatSpec with Matchers {

  behavior of "WdlNamespaces with subworkflows"

  it should "support WDL to WOM conversion of subworkflow calls" in {
    val outerWdl =
      """import "import_me.wdl" as import_me
        |
        |workflow outer {
        |  Int x
        |  call import_me.inner as inner { input: i = x }
        |  output {
        |    Array[String] out = inner.out
        |  }
        |}""".stripMargin

    val innerWdl =
      """task foo {
        |  Int i
        |  command {
        |    echo ${i}
        |  }
        |  output {
        |    String out = read_string(stdout())
        |  }
        |}
        |
        |workflow inner {
        |  Int i
        |  call foo { input: i = i }
        |  output {
        |    String out = foo.out
        |    Int x = 5050
        |  }
        |}
      """.stripMargin


    def innerResolver: ImportResolver = _ => innerWdl

    val namespace = WdlNamespace.loadUsingSource(
      workflowSource = outerWdl,
      resource = None,
      importResolver = Some(Seq(innerResolver))).get.asInstanceOf[WdlNamespaceWithWorkflow]
    import lenthall.validation.ErrorOr.ShortCircuitingFlatMap

    val outerWorkflowGraph = namespace.womExecutable.flatMap(_.graph)

    outerWorkflowGraph match {
      case Valid(g) => validateOuter(g)
      case Invalid(errors) => fail(s"Unable to build wom version of workflow with subworkflow from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }

    def validateOuter(workflowGraph: Graph) = {
      // One input, x
      workflowGraph.inputNodes.map(_.name) should be(Set("x"))
      val calls = workflowGraph.calls
      calls.map(_.name) should be(Set("inner"))

      // One workflow call, "inner"
      val innerCall = calls.head.asInstanceOf[WorkflowCallNode]
      innerCall.name should be("inner")
      innerCall.upstream should be(Set(workflowGraph.inputNodes.head))

      // One output, "out"
      workflowGraph.outputNodes.map(_.name) should be(Set("out"))
      workflowGraph.outputNodes.head.womType should be(WdlStringType)
      workflowGraph.outputNodes.foreach(_.upstream should be(Set(innerCall)))

      validateInner(innerCall.callable.innerGraph)
    }

    def validateInner(innerGraph: Graph) = {
      innerGraph.inputNodes.map(_.name) should be(Set("i"))
      val calls = innerGraph.calls
      calls.map(_.name) should be(Set("foo"))

      val fooCall = calls.head.asInstanceOf[TaskCallNode]
      fooCall.upstream should be(Set(innerGraph.inputNodes.head))

      innerGraph.outputNodes.map(_.name) should be(Set("out", "x"))
      val outOutput = innerGraph.outputNodes.find(_.name == "out").get
      val xOutput = innerGraph.outputNodes.find(_.name == "x").get

      outOutput.womType should be(WdlStringType)
      outOutput.upstream should be(Set(fooCall))

      xOutput.womType should be(WdlIntegerType)
      xOutput.upstream should be(Set.empty)
    }
  }

  it should "support WDL to WOM conversion of subworkflows in scatters" in {
    val outerWdl =
      """import "import_me.wdl" as import_me
        |
        |workflow outer {
        |  Array[Int] xs
        |  scatter (x in xs) {
        |    call import_me.inner as inner { input: i = x }
        |  }
        |  output {
        |    Array[String] outs = inner.out
        |  }
        |}""".stripMargin

    val innerWdl =
      """task foo {
        |  Int i
        |  command {
        |    echo ${i}
        |  }
        |  output {
        |    String out = read_string(stdout())
        |  }
        |}
        |
        |workflow inner {
        |  Int i
        |  call foo { input: i = i }
        |  output {
        |    String out = foo.out
        |  }
        |}
      """.stripMargin


    def innerResolver: ImportResolver = _ => innerWdl

    val namespace = WdlNamespace.loadUsingSource(
      workflowSource = outerWdl,
      resource = None,
      importResolver = Some(Seq(innerResolver))).get.asInstanceOf[WdlNamespaceWithWorkflow]
    import lenthall.validation.ErrorOr.ShortCircuitingFlatMap

    val outerWorkflowGraph = namespace.womExecutable.flatMap(_.graph)

    outerWorkflowGraph match {
      case Valid(g) => validateOuter(g)
      case Invalid(errors) => fail(s"Unable to build wom version of workflow with subworkflow from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }

    def validateOuter(workflowGraph: Graph) = {
      workflowGraph.inputNodes.map(_.name) should be(Set("xs"))

      val scatter = workflowGraph.scatters.head
      scatter.upstream should be(Set(workflowGraph.inputNodes.head))
      scatter.outputPorts.map(_.name) should be(Set("inner.out"))
      scatter.outputPorts.head.womType should be(WdlArrayType(WdlStringType))

      workflowGraph.outputNodes.map(_.name) should be(Set("outs"))
      workflowGraph.outputNodes.head.womType should be(WdlArrayType(WdlStringType))
      workflowGraph.outputNodes.foreach(_.upstream should be(Set(scatter)))
    }
  }

}