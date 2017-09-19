package wdl4s.wom.graph

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.types.{WdlFileType, WdlIntegerType, WdlStringType}
import wdl4s.wom.RuntimeAttributes
import wdl4s.wom.callable.Callable.{OutputDefinition, RequiredInputDefinition}
import wdl4s.wom.callable.{TaskDefinition, WorkflowDefinition}
import wdl4s.wom.graph.CallNode.CallNodeAndNewInputs

class GraphSpec extends FlatSpec with Matchers {
  behavior of "Graph"

  def makeThreeStep: Graph = {
    val taskDefinition_ps = TaskDefinition(
      name = "ps",
      commandTemplate = null,
      runtimeAttributes = RuntimeAttributes(attributes = Map.empty),
      meta = Map.empty,
      parameterMeta = Map.empty,
      outputs = Set(OutputDefinition("procs", WdlFileType, null)),
      inputs = List.empty
    )

    val taskDefinition_cgrep = TaskDefinition(
      name = "cgrep",
      commandTemplate = null,
      runtimeAttributes = RuntimeAttributes(attributes = Map.empty),
      meta = Map.empty,
      parameterMeta = Map.empty,
      outputs = Set(OutputDefinition("count", WdlIntegerType, null)),
      inputs = List(RequiredInputDefinition("pattern", WdlStringType), RequiredInputDefinition("in_file", WdlFileType))
    )

    val taskDefinition_wc = TaskDefinition(
      name = "wc",
      commandTemplate = null,
      runtimeAttributes = RuntimeAttributes(attributes = Map.empty),
      meta = Map.empty,
      parameterMeta = Map.empty,
      outputs = Set(OutputDefinition("count", WdlIntegerType, null)),
      inputs = List(RequiredInputDefinition("in_file", WdlFileType))
    )

    val CallNodeAndNewInputs(psCall, psGraphInputs) = CallNode.callWithInputs("ps", taskDefinition_ps, Map.empty, Set.empty).getOrElse(fail("Unable to call ps"))
    val ps_procsOutputPort = psCall.outputByName("procs").getOrElse(fail("Unexpectedly unable to find 'ps.procs' output"))
    val CallNodeAndNewInputs(cgrepCall, cgrepGraphInputs) = CallNode.callWithInputs("cgrep", taskDefinition_cgrep, Map("in_file" -> ps_procsOutputPort), Set.empty).getOrElse(fail("Unable to call cgrep"))
    val cgrep_countOutputPort = cgrepCall.outputByName("count").getOrElse(fail("Unexpectedly unable to find 'cgrep.count' output"))
    val CallNodeAndNewInputs(wcCall, wcGraphInputs) = CallNode.callWithInputs("wc", taskDefinition_wc, Map("in_file" -> ps_procsOutputPort), Set.empty).getOrElse(fail("Unable to call wc"))
    val wc_countOutputPort = wcCall.outputByName("count").getOrElse(fail("Unexpectedly unable to find 'wc.count' output"))

    val psProcsOutputNode = PortBasedGraphOutputNode("ps.procs", WdlFileType, ps_procsOutputPort)
    val cgrepCountOutputNode = PortBasedGraphOutputNode("cgrep.count", WdlIntegerType, cgrep_countOutputPort)
    val wcCountOutputNode = PortBasedGraphOutputNode("wc.count", WdlIntegerType, wc_countOutputPort)

    val graphNodes: Set[GraphNode] =
      Set[GraphNode](psCall, cgrepCall, wcCall, psProcsOutputNode, cgrepCountOutputNode, wcCountOutputNode)
        .union(psGraphInputs.toSet[GraphNode])
        .union(cgrepGraphInputs.toSet[GraphNode])
        .union(wcGraphInputs.toSet[GraphNode])

    Graph.validateAndConstruct(graphNodes) match {
      case Valid(wg) => wg
      case Invalid(errors) => fail(s"Unable to validate graph: ${errors.toList.mkString("\n", "\n", "\n")}")
    }
  }

  it should "be able to represent three step" in {
    val workflowGraph = makeThreeStep

    workflowGraph.nodes collect { case gin: GraphInputNode => gin.name } should be(Set("cgrep.pattern"))
    workflowGraph.nodes collect { case gon: PortBasedGraphOutputNode => gon.name } should be(Set("wc.count", "cgrep.count", "ps.procs"))
    workflowGraph.nodes collect { case cn: CallNode => cn.name } should be(Set("wc", "cgrep", "ps"))
  }

  it should "be able to represent calls to sub-workflows" in {
    val threeStepWorkflow = WorkflowDefinition("three_step", makeThreeStep, Map.empty, Map.empty, List.empty)
    val CallNodeAndNewInputs(threeStepCall, threeStepInputs) = CallNode.callWithInputs("three_step", threeStepWorkflow, Map.empty, Set.empty).getOrElse(fail("Unable to call three_step"))

    // This is painful manually, but it's not up to WOM to decide which subworkflow outputs are forwarded through:
    val psProcsOutputNode = PortBasedGraphOutputNode("three_step.ps.procs", WdlFileType, threeStepCall.outputByName("ps.procs").getOrElse(fail("Subworkflow didn't expose the ps.procs output")))
    val cgrepCountOutputNode = PortBasedGraphOutputNode("three_step.cgrep.count", WdlIntegerType, threeStepCall.outputByName("cgrep.count").getOrElse(fail("Subworkflow didn't expose the cgrep.count output")))
    val wcCountOutputNode = PortBasedGraphOutputNode("three_step.wc.count", WdlIntegerType, threeStepCall.outputByName("wc.count").getOrElse(fail("Subworkflow didn't expose the wc.count output")))

    val workflowGraph = Graph.validateAndConstruct(Set[GraphNode](threeStepCall, psProcsOutputNode, cgrepCountOutputNode, wcCountOutputNode).union(threeStepInputs.toSet[GraphNode])) match {
      case Valid(wg) => wg
      case Invalid(errors) => fail(s"Unable to validate graph: ${errors.toList.mkString("\n", "\n", "\n")}")
    }

    workflowGraph.nodes collect { case gin: GraphInputNode => gin.name } should be(Set("three_step.cgrep.pattern"))
    workflowGraph.nodes collect { case gon: GraphOutputNode => gon.name } should be(Set("three_step.wc.count", "three_step.cgrep.count", "three_step.ps.procs"))
    workflowGraph.nodes collect { case cn: CallNode => cn.name } should be(Set("three_step"))
  }
}
