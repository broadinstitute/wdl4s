package wdl4s.cwl

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.traverse._
import cats.instances.list._
import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.types.{WdlFileType, WdlStringType}
import wdl4s.wdl.{WdlNamespace, WdlNamespaceWithWorkflow}
import wdl4s.wom.callable.Callable.RequiredInputDefinition
import wdl4s.wom.callable.{TaskDefinition, WorkflowDefinition}
import wdl4s.wom.executable.Executable
import wdl4s.wom.graph.{CallNode, GraphInputNode, GraphOutputNode, RequiredGraphInputNode}

class CwlWorkflowWomSpec extends FlatSpec with Matchers {
  "munging the workflow output id" should "remove the filename" in {
    val id = "file:///home/dan/common-workflow-language/v1.0/examples/1st-workflow.cwl#untar/example_out"

    val out = WorkflowOutputsToOutputDefinition.mungeId(id)

    out shouldBe "example_out"
  }

  "munging the runnable id" should "remove the filename" in {
    val id = "file:///home/dan/common-workflow-language/v1.0/examples/tar-param.cwl#example_out"

    val out = RunToTypeMap.mungeId(id)

    out shouldBe "example_out"
  }


  "A Cwl object for 1st-tool" should "convert to WOM" in {
    val firstTool =
      """
cwlVersion: v1.0
class: CommandLineTool
baseCommand: echo
inputs:
- type: string
  inputBinding:
    position: 1
  id: message
outputs: []
""".stripMargin

    CwlCodecs.decodeCwl(firstTool) match {
      case Right(clt:CommandLineTool) =>
        clt.womExecutable match{
          case Valid(wom) =>
            wom.entryPoint match{
              case workflow: TaskDefinition =>

                workflow.inputs shouldBe Set(RequiredInputDefinition("message", WdlStringType))

                workflow.graph.map{
                  graph =>
                    graph.nodes.collect{ case gin: GraphInputNode => gin.name } should be(Set("echo.message"))
                    graph.nodes collect { case cn: CallNode => cn.name } should be(Set("echo"))

                    graph.nodes.collectFirst{ case echo: CallNode if echo.name == "echo" => echo }.get.
                      upstream shouldBe Set(RequiredGraphInputNode("echo.message", WdlStringType))
                }

              case  _ => fail("not a workflow")
            }
        }
      case Left(error) => fail(s"did not parse!  $error")
    }
  }

  "Cwl for 1st workflow" should "convert to WOM" in {
    val firstWorkflow =
      s"""
cwlVersion: "v1.0"
class: "Workflow"
inputs:
  - type: "string"
    id: "file:///home/dan/wdl4s/r.cwl#ex"
  - type: "File"
    id: "file:///home/dan/wdl4s/r.cwl#inp"
outputs:
  - type: "File"
    outputSource: "file:///home/dan/wdl4s/r.cwl#compile/classfile"
    id: "file:///home/dan/wdl4s/r.cwl#classout"
steps:
  - run: "arguments.cwl"
    in:
      -
        source: "file:///home/dan/wdl4s/r.cwl#untar/example_out"
        id: "file:///home/dan/wdl4s/r.cwl#compile/src"
    out:
      - "file:///home/dan/wdl4s/r.cwl#compile/classfile"
    id: "file:///home/dan/wdl4s/r.cwl#compile"
  - run: "tar-param.cwl"
    in:
      -
        source: "file:///home/dan/wdl4s/r.cwl#ex"
        id: "file:///home/dan/wdl4s/r.cwl#untar/extractfile"
      -
        source: "file:///home/dan/wdl4s/r.cwl#inp"
        id: "file:///home/dan/wdl4s/r.cwl#untar/tarfile"
    out:
      - "file:///home/dan/wdl4s/r.cwl#untar/example_out"
    id: "file:///home/dan/wdl4s/r.cwl#untar"
id: "file:///home/dan/wdl4s/r.cwl"
name: "file:///home/dan/wdl4s/r.cwl"

""".stripMargin

    import CwlCodecs._


    decodeCwlX(firstWorkflow) match {
      case Right((workflow:Workflow, map:Map[String, CwlFile])) =>
        workflow.womExecutable(map) match {
          case Valid(ex) => validateWom(ex)
          case e => fail(s"executable was not created $e")
        }
      case Left(error) => fail(s"did not parse!  $error")
    }

    def validateWom(ex: Executable) = {
      ex match {
        case Executable(wf: WorkflowDefinition) =>
          val nodes = wf.innerGraph.withDefaultOutputs.nodes

          nodes.collect{
            case gin: GraphInputNode => gin.name
          } should be(Set("file:///home/dan/wdl4s/r.cwl#ex", "file:///home/dan/wdl4s/r.cwl#inp"))

          nodes collect {
            case cn: CallNode => cn.name
          } should be(Set("file:///home/dan/wdl4s/r.cwl#compile", "file:///home/dan/wdl4s/r.cwl#untar"))

          nodes.collectFirst{
            case tarParam: CallNode if tarParam.name == "file:///home/dan/wdl4s/r.cwl#untar" => tarParam
          }.get.
            upstream shouldBe Set(
              RequiredGraphInputNode("file:///home/dan/wdl4s/r.cwl#ex", WdlStringType),
              RequiredGraphInputNode("file:///home/dan/wdl4s/r.cwl#inp", WdlFileType))


          nodes.collectFirst{
            case compile: CallNode if compile.name == "file:///home/dan/wdl4s/r.cwl#compile" => compile
          }.get.inputPorts.map(_.upstream).head.name shouldBe "file:///home/dan/wdl4s/r.cwl#untar/example_out"
      }
    }

  }

  "A WdlNamespace for 3step" should "provide conversion to WOM" in {
    val threeStep =
      """
cwlVersion: v1.0
class: Workflow
inputs:
- id: file:///Users/danb/wdl4s/r.cwl#pattern
  type: string
outputs:
- id: file:///Users/danb/wdl4s/r.cwl#cgrep-stdOut
  outputSource: file:///Users/danb/wdl4s/r.cwl#cgrep/cgrep-stdOut
  type: File
- id: file:///Users/danb/wdl4s/r.cwl#wc-stdOut
  outputSource: file:///Users/danb/wdl4s/r.cwl#wc/wc-stdOut
  type: File
steps:
- id: file:///Users/danb/wdl4s/r.cwl#ps
  in: []
  out:
  - file:///Users/danb/wdl4s/r.cwl#ps/ps-stdOut
  run:
    inputs: []
    outputs:
    - id: file:///Users/danb/wdl4s/r.cwl#ps/0b4ba500-5584-4fed-a831-9fa6f914ad3f/ps-stdOut
      outputBinding:
        glob: ps-stdOut.txt
      type: File
    class: CommandLineTool
    baseCommand: ps
    stdout: ps-stdOut.txt
    id: file:///Users/danb/wdl4s/r.cwl#ps/0b4ba500-5584-4fed-a831-9fa6f914ad3f
- id: file:///Users/danb/wdl4s/r.cwl#cgrep
  in:
  - id: file:///Users/danb/wdl4s/r.cwl#cgrep/pattern
    source: file:///Users/danb/wdl4s/r.cwl#pattern
  - id: file:///Users/danb/wdl4s/r.cwl#cgrep/file
    source: file:///Users/danb/wdl4s/r.cwl#ps/ps-stdOut
  out:
  - id: file:///Users/danb/wdl4s/r.cwl#cgrep/cgrep-stdOut
  run:
    inputs:
    - id: file:///Users/danb/wdl4s/r.cwl#cgrep/09f8bcac-a91a-49d5-afb6-2f1b1294e875/pattern
      type: string
    - id: file:///Users/danb/wdl4s/r.cwl#cgrep/09f8bcac-a91a-49d5-afb6-2f1b1294e875/file
      type: File
    outputs:
    - id: file:///Users/danb/wdl4s/r.cwl#cgrep/09f8bcac-a91a-49d5-afb6-2f1b1294e875/cgrep-stdOut
      outputBinding:
        glob: cgrep-stdOut.txt
      type: File
    class: CommandLineTool
    requirements:
    - class: ShellCommandRequirement
    - class: InlineJavascriptRequirement
    arguments:
    - valueFrom: grep
      shellQuote: false
    - valueFrom: $(inputs.pattern).
      shellQuote: false
    - valueFrom: $(inputs.file)
      shellQuote: false
    - valueFrom: '|'
      shellQuote: false
    - valueFrom: wc
      shellQuote: false
    - valueFrom: -l
      shellQuote: false
    stdout: cgrep-stdOut.txt
    id: file:///Users/danb/wdl4s/r.cwl#cgrep/09f8bcac-a91a-49d5-afb6-2f1b1294e875
- id: file:///Users/danb/wdl4s/r.cwl#wc
  in:
  - id: file:///Users/danb/wdl4s/r.cwl#wc/file
    source: file:///Users/danb/wdl4s/r.cwl#ps/ps-stdOut
  out:
  - id: file:///Users/danb/wdl4s/r.cwl#wc/wc-stdOut
  run:
    inputs:
    - id: file:///Users/danb/wdl4s/r.cwl#wc/45d98851-7bfe-473e-ab24-aac922553f3e/file
      type: File
    outputs:
    - id: file:///Users/danb/wdl4s/r.cwl#wc/45d98851-7bfe-473e-ab24-aac922553f3e/wc-stdOut
      outputBinding:
        glob: wc-stdOut.txt
      type: File
    class: CommandLineTool
    requirements:
    - class: ShellCommandRequirement
    - class: InlineJavascriptRequirement
    arguments:
    - valueFrom: cat
      shellQuote: false
    - valueFrom: $(inputs.file)
      shellQuote: false
    - valueFrom: '|'
      shellQuote: false
    - valueFrom: wc
      shellQuote: false
    - valueFrom: -l
      shellQuote: false
    stdout: wc-stdOut.txt
    id: file:///Users/danb/wdl4s/r.cwl#wc/45d98851-7bfe-473e-ab24-aac922553f3e
id: file:///Users/danb/wdl4s/r.cwl
      """.stripMargin

    val workflow = CwlCodecs.decodeCwl(threeStep) match {
      case Right(wf: Workflow) => wf
      case o => fail(s"didn't parse a workflow! $o")
    }
    val wf = workflow.womExecutable(Map.empty) match {
      case Valid(Executable(wf: WorkflowDefinition)) => wf
      case o => fail(s"invalid executable $o")
    }

    val workflowGraph = wf.graph match {
      case Valid(g) => g
      case Invalid(errors) => fail(s"Unable to build wom version of 3step from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }

    workflowGraph.nodes collect { case gin: GraphInputNode => gin.name } should be(Set("cgrep.pattern"))
    workflowGraph.nodes collect { case gon: GraphOutputNode => gon.name } should be(Set("wc.count", "cgrep.count", "ps.procs"))
    workflowGraph.nodes collect { case cn: CallNode => cn.name } should be(Set("wc", "cgrep", "ps"))

    val ps = workflowGraph.nodes.collectFirst({ case ps: CallNode if ps.name == "ps" => ps }).get
    val cgrep = workflowGraph.nodes.collectFirst({ case cgrep: CallNode if cgrep.name == "cgrep" => cgrep }).get
    val cgrepPatternInput = workflowGraph.nodes.collectFirst({ case cgrepInput: GraphInputNode if cgrepInput.name == "cgrep.pattern" => cgrepInput }).get
    val wc = workflowGraph.nodes.collectFirst({ case wc: CallNode if wc.name == "wc" => wc }).get

    ps.upstream shouldBe empty
    cgrep.upstream shouldBe Set(ps, cgrepPatternInput)
    wc.upstream shouldBe Set(ps)
  }

}
