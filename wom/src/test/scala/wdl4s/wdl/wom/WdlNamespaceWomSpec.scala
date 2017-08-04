package wdl4s.wdl.wom

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.{WdlNamespace, WdlNamespaceWithWorkflow}
import wdl4s.wom.graph.{CallNode, GraphInputNode, GraphOutputNode}

class WdlNamespaceWomSpec extends FlatSpec with Matchers {
  
  "A WdlNamespace for 4step" should "provide conversion to WOM" in {
    val threeStep =
      """
        |task ps {
        |  command {
        |    ps
        |  }
        |  output {
        |    File procs = stdout()
        |  }
        |}
        |
        |task cgrep {
        |  String pattern
        |  File in_file
        |
        |  command {
        |    grep '${pattern}' ${in_file} | wc -l
        |  }
        |  output {
        |    Int count = read_int(stdout())
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
        |}
        |
        |task echo {
        |  Int i
        |  command {
        |    echo ${i}
        |  }
        |  output {
        |    String o = read_string(stdout())
        |  }
        |}
        |
        |workflow four_step {
        |  call ps
        |  call cgrep {
        |    input: in_file = ps.procs
        |  }
        |  call wc {
        |    input: in_file = ps.procs
        |  }
        |  call echo {
        |    input: i = cgrep.count + wc.count
        |  }
        |}
      """.stripMargin

    val namespace = WdlNamespace.loadUsingSource(threeStep, None, None).get.asInstanceOf[WdlNamespaceWithWorkflow]
    val wom4Step = namespace.womExecutable
    
    val workflowGraph = wom4Step.graph match {
      case Valid(g) => g
      case Invalid(errors) => fail(s"Unable to build wom version of 3step from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }
    
    workflowGraph.nodes collect { case gin: GraphInputNode => gin.name } should be(Set("cgrep.pattern"))
    workflowGraph.nodes collect { case gon: GraphOutputNode => gon.name } should be(Set("wc.count", "cgrep.count", "ps.procs", "echo.o"))
    workflowGraph.nodes collect { case cn: CallNode => cn.name } should be(Set("wc", "cgrep", "ps", "echo"))
    
    val ps = workflowGraph.nodes.collectFirst({ case ps: CallNode if ps.name == "ps" => ps }).get
    val cgrep = workflowGraph.nodes.collectFirst({ case cgrep: CallNode if cgrep.name == "cgrep" => cgrep }).get
    val cgrepPatternInput = workflowGraph.nodes.collectFirst({ case cgrepInput: GraphInputNode if cgrepInput.name == "cgrep.pattern" => cgrepInput }).get
    val wc = workflowGraph.nodes.collectFirst({ case wc: CallNode if wc.name == "wc" => wc }).get
    val echo = workflowGraph.nodes.collectFirst({ case echo: CallNode if echo.name == "echo" => echo }).get
    
    ps.upstream shouldBe empty
    cgrep.upstream shouldBe Set(ps, cgrepPatternInput)
    wc.upstream shouldBe Set(ps)
    echo.upstream shouldBe Set(cgrep, wc)
  }

}
