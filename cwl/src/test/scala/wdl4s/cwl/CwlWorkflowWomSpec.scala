package wdl4s.cwl

import cats.syntax.either._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import shapeless._
import wdl4s.cwl.CwlDecoder._
import wdl4s.wdl.types.{WdlFileType, WdlStringType}
import wdl4s.wom.WomMatchers._
import wdl4s.wom.callable.Callable.RequiredInputDefinition
import wdl4s.wom.callable.{TaskDefinition, WorkflowDefinition}
import wdl4s.wom.executable.Executable
import wdl4s.wom.graph.GraphNodePort.OutputPort
import wdl4s.wom.graph._
import eu.timepit.refined._

class CwlWorkflowWomSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  import TestSetup._

  "munging the runnable id" should "remove the filename" in {
    val id = "file:///home/dan/common-workflow-language/v1.0/examples/tar-param.cwl#example_out"
    val out = RunOutputsToTypeMap.mungeId(id)

    out shouldBe "example_out"
  }

  "munging runnable output id " should "be able to skip the path args" in {
    val id = "file:///home/dan/common-workflow-language/v1.0/examples/tar-param.cwl#ps/0b4ba500-5584-4fed-a831-9fa6f914ad3f/ps-stdOut"
    val out = RunOutputsToTypeMap.mungeId(id)

    out shouldBe "ps-stdOut"
  }


  "A Cwl object for 1st-tool" should "convert to WOM" in {
    val validateWom: Executable => Unit =
      _.entryPoint match {
        case taskDefinition: TaskDefinition =>
          taskDefinition.inputs shouldBe List(RequiredInputDefinition(s"message", WdlStringType))
          ()

        case _ => fail("not a task definition")
      }

    (for {
      clt <- decodeAllCwl(rootPath/"1st-tool.cwl").
              map(_.select[CommandLineTool].get).
              value.
              unsafeRunSync
      wom <- clt.womExecutable
    } yield validateWom(wom)).leftMap(e => throw new RuntimeException(s"error! $e"))
  }

  "Cwl for 1st workflow" should "convert to WOM" in {
    (for {
      wf <- decodeAllCwl(rootPath/"1st-workflow.cwl").
              value.
              unsafeRunSync.
              map(_.select[Workflow].get)

      ex <- wf.womExecutable
    } yield validateWom(ex)).leftMap(e => throw new RuntimeException(s"error! $e"))

    def validateWom(ex: Executable) = {
      ex match {
        case Executable(wf: WorkflowDefinition) =>
          val nodes = wf.innerGraph.nodes

          nodes collect {
            case gin: GraphInputNode => gin.name
          } should be(Set(s"file://$rootPath/1st-workflow.cwl#ex", s"file://$rootPath/1st-workflow.cwl#inp"))

          nodes collect {
            case cn: CallNode => cn.name
          } should be(Set("compile", "untar"))

          val untarUpstream = nodes.collectFirst {
            case tarParam: CallNode if tarParam.name == s"untar" => tarParam
          }.get.
            upstream
          
          untarUpstream should have size 2
          untarUpstream.collectFirst({
            case exprNode: ExpressionNode if exprNode.name == s"file://$rootPath/1st-workflow.cwl#untar/extractfile" =>
              exprNode.inputPorts.head.upstream.graphNode shouldBe RequiredGraphInputNode(s"file://$rootPath/1st-workflow.cwl#ex", WdlStringType)
          }).getOrElse(fail("Can't find expression node for ex"))
          
          untarUpstream.collectFirst({
            case exprNode: ExpressionNode if exprNode.name == s"file://$rootPath/1st-workflow.cwl#untar/tarfile" =>
              exprNode.inputPorts.head.upstream.graphNode shouldBe RequiredGraphInputNode(s"file://$rootPath/1st-workflow.cwl#inp", WdlFileType)
          }).getOrElse(fail("Can't find expression node for inp"))

          val compileUpstreamExpressionPort = nodes.collectFirst {
            case compile: CallNode if compile.name == s"compile" => compile
          }.get.inputPorts.map(_.upstream).head

          compileUpstreamExpressionPort.name shouldBe s"file://$rootPath/1st-workflow.cwl#compile/src"
          compileUpstreamExpressionPort.graphNode.asInstanceOf[ExpressionNode].inputPorts.head.upstream.name shouldBe s"example_out"

          nodes.collect {
            case c: PortBasedGraphOutputNode => c
          }.map(_.name) shouldBe Set(s"file://$rootPath/1st-workflow.cwl#classout")
        case Executable(wth: Any) => fail(s"Parsed unexpected Callable: $wth")
      }
    }
  }

  behavior of "A decoded CWL 3step"

  private val stringOrExpressionTests = Table(
    ("index", "result"),
    (0, Coproduct[CommandLineTool.StringOrExpression]("grep")),
    (1, Coproduct[CommandLineTool.StringOrExpression](refineMV[MatchesECMAScriptExpression]("$(inputs.pattern)"))),
    (2, Coproduct[CommandLineTool.StringOrExpression](refineMV[MatchesECMAScriptExpression]("$(inputs.file)"))),
    (3, Coproduct[CommandLineTool.StringOrExpression]("|")),
    (4, Coproduct[CommandLineTool.StringOrExpression]("wc")),
    (5, Coproduct[CommandLineTool.StringOrExpression]("-l"))
  )

  private def getTestName(stringOrExpression: CommandLineTool.StringOrExpression): String = {
    object StringOrExpressionToTestName extends Poly1 {
      implicit def caseECMAScriptExpression: Case.Aux[ECMAScriptExpression, String] = {
        at[ECMAScriptExpression] { ecmaScriptExpression => s"expression ${ecmaScriptExpression.value}" }
      }

      implicit def caseString: Case.Aux[String, String] = {
        at[String] { string => s"string $string" }
      }
    }

    stringOrExpression.fold(StringOrExpressionToTestName)
  }

  private lazy val commandLineTool: CommandLineTool = {
    val wf = decodeAllCwl(rootPath / "three_step.cwl").map {
      _.select[Workflow].get
    }.value.unsafeRunSync.fold(error => throw new RuntimeException(s"broken parse! msg was $error"), identity)

    // The second step (aka 1) step should be cgrep
    val run: WorkflowStep.Run = wf.steps.apply(1).run
    val commandLineTool: CommandLineTool = run.select[CommandLineTool].getOrElse(fail(s"$run wasn't a CommandLineTool"))

    commandLineTool.id.get should include("cgrep")
    commandLineTool
  }

  forAll(stringOrExpressionTests) { (index, expected) =>
    it should s"correctly identify the ${getTestName(expected)}" in {
      val argument: CommandLineTool.Argument = commandLineTool.arguments.get.apply(index)
      val commandLineBinding: CommandLineBinding = argument.select[CommandLineBinding]
        .getOrElse(fail(s"$argument wasn't a CommandLineBinding"))
      val stringOrExpression: CommandLineTool.StringOrExpression = commandLineBinding.valueFrom
        .getOrElse(fail(s"valueFrom missing in $commandLineBinding"))
      stringOrExpression should be(expected)
    }
  }

  "A CwlNamespace for 3step" should "provide conversion to WOM" in {

    val wf = decodeAllCwl(rootPath/"three_step.cwl").map {
      _.select[Workflow].get
    }.value.unsafeRunSync.fold(error => throw new RuntimeException(s"broken parse! msg was $error"), identity)

    val wfd = wf.womExecutable match {
      case Right(Executable(wf: WorkflowDefinition)) => wf
      case Left(o) => fail(s"Workflow definition was not produced correctly: ${o.toList.mkString(", ")}")
      case Right(Executable(callable)) => fail(s"produced $callable when a Workflow Definition was expected!")
    }

    val nodes = wfd.innerGraph.nodes

    val graphInputNodes = nodes collect { case gin: GraphInputNode => gin }
    graphInputNodes should have size 1
    val patternInputNode = graphInputNodes.head
    patternInputNode.name should be("file:///Users/danb/wdl4s/r.cwl#pattern")

    nodes collect { case gon: GraphOutputNode => gon.name } should be(Set(
      "file:///Users/danb/wdl4s/r.cwl#cgrep-count",
      "file:///Users/danb/wdl4s/r.cwl#wc-count"
    ))

    nodes collect { case cn: CallNode => cn.name } should be(Set("ps", "cgrep", "wc"))

    val ps = nodes.collectFirst({ case ps: CallNode if ps.name == "ps" => ps }).get
    val cgrep = nodes.collectFirst({ case cgrep: CallNode if cgrep.name == "cgrep" => cgrep }).get
    val cgrepPatternInput = nodes.collectFirst({ case cgrepInput: GraphInputNode if cgrepInput.name == "file:///Users/danb/wdl4s/r.cwl#pattern" => cgrepInput }).get
    val cgrepFileExpression = nodes.collectFirst({ case cgrepInput: ExpressionNode if cgrepInput.name == "file:///Users/danb/wdl4s/r.cwl#cgrep/file" => cgrepInput }).get
    val cgrepPatternExpression = nodes.collectFirst({ case cgrepInput: ExpressionNode if cgrepInput.name == "file:///Users/danb/wdl4s/r.cwl#cgrep/pattern" => cgrepInput }).get
    val wc = nodes.collectFirst({ case wc: CallNode if wc.name == "wc" => wc }).get
    val wcFileExpression = nodes.collectFirst({ case wcInput: ExpressionNode if wcInput.name == "file:///Users/danb/wdl4s/r.cwl#wc/file" => wcInput }).get

    ps.upstream shouldBe empty

    cgrep.upstream should contain theSameElementsAs Set(cgrepFileExpression, cgrepPatternExpression)
    wc.upstream should contain theSameElementsAs Set(wcFileExpression)

    // Check that expressions input ports point to the right output port
    cgrepPatternExpression.inputPorts.head.upstream should be theSameInstanceAs cgrepPatternInput.singleOutputPort
    cgrepFileExpression.inputPorts.head.upstream should be theSameInstanceAs ps.outputPorts.head
    wcFileExpression.inputPorts.head.upstream should be theSameInstanceAs ps.outputPorts.head
    
    // Check that the inputDefinitionMappings are correct
    ps.inputDefinitionMappings shouldBe empty
    cgrep.inputDefinitionMappings should have size 2
    
    val cgrepFileInputDef = cgrep.callable.inputs.find(_.name == "file").get
    cgrep.inputDefinitionMappings(cgrepFileInputDef).select[OutputPort].get should be theSameInstanceAs cgrepFileExpression.singleExpressionOutputPort
    
    val cgrepPatternInputDef = cgrep.callable.inputs.find(_.name == "pattern").get
    cgrep.inputDefinitionMappings(cgrepPatternInputDef).select[OutputPort].get should be theSameInstanceAs cgrepPatternExpression.singleExpressionOutputPort
  }
}
