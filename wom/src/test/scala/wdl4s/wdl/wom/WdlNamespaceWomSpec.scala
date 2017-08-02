package wdl4s.wdl.wom

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.{WdlNamespace, WdlNamespaceWithWorkflow}
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.{CallNode, GraphInputNode, GraphOutputNode}

class WdlNamespaceWomSpec extends FlatSpec with Matchers {
  
  "A WdlNamespace for 3step" should "provide conversion to WOM" in {
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
        |workflow three_step {
        |  call ps
        |  call cgrep {
        |    input: in_file = ps.procs + "hello"
        |  }
        |  call wc {
        |    input: in_file = ps.procs
        |  }
        |}
      """.stripMargin

    val namespace = WdlNamespace.loadUsingSource(threeStep, None, None).get.asInstanceOf[WdlNamespaceWithWorkflow]
    val wom3Step = namespace.womExecutable
    
    val workflowGraph = wom3Step.graph match {
      case Valid(g) => g
      case Invalid(errors) => fail(s"Unable to build wom version of 3step from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }
    
    workflowGraph.nodes collect { case gin: GraphInputNode => gin.name } should be(Set("cgrep.pattern"))
    workflowGraph.nodes collect { case gon: GraphOutputNode => gon.name } should be(Set("wc.count", "cgrep.count", "ps.procs"))
    workflowGraph.nodes collect { case cn: CallNode => cn.name } should be(Set("wc", "cgrep", "ps"))
    
    val ps = workflowGraph.nodes.collectFirst({ case ps: CallNode if ps.name == "ps" => ps }).get
    val cgrep = workflowGraph.nodes.collectFirst({ case cgrep: CallNode if cgrep.name == "cgrep" => cgrep }).get
    val cgrepInFileExpression = workflowGraph.nodes.collectFirst({ case cgrepExpression: WomExpression if cgrepExpression.name == "cgrep.in_file.expression" => cgrepExpression }).get
    val cgrepPatternInput = workflowGraph.nodes.collectFirst({ case cgrepInput: GraphInputNode if cgrepInput.name == "cgrep.pattern" => cgrepInput }).get
    val wcInFileExpression = workflowGraph.nodes.collectFirst({ case wcExpression: WomExpression if wcExpression.name == "wc.in_file.expression" => wcExpression }).get
    val wc = workflowGraph.nodes.collectFirst({ case wc: CallNode if wc.name == "wc" => wc }).get
    
    ps.upstream shouldBe empty
    cgrepInFileExpression.upstream shouldBe Set(ps)
    cgrep.upstream shouldBe Set(cgrepInFileExpression, cgrepPatternInput)
    wcInFileExpression.upstream shouldBe Set(ps)
    wc.upstream shouldBe Set(wcInFileExpression)
  }

}
