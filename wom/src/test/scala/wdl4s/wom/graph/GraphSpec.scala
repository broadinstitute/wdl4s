package wom.graph

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.Coproduct
import wdl.types.{WdlFileType, WdlIntegerType, WdlOptionalType, WdlStringType}
import wdl.values.{WdlOptionalValue, WdlString, WdlValue}
import wom.RuntimeAttributes
import wom.callable.Callable.{OutputDefinition, RequiredInputDefinition}
import wom.callable.{TaskDefinition, WorkflowDefinition}
import wom.expression.{PlaceholderWomExpression, WomExpression}
import wom.graph.CallNode.{CallNodeAndNewNodes, CallNodeBuilder, InputDefinitionFold, InputDefinitionPointer}
import wom.graph.Graph.ResolvedWorkflowInput
import wom.graph.GraphNodePort.OutputPort

class GraphSpec extends FlatSpec with Matchers {
  behavior of "Graph"

  def makeThreeStep: Graph = {
    val taskDefinition_ps = TaskDefinition(
      name = "ps",
      commandTemplate = null,
      runtimeAttributes = RuntimeAttributes(attributes = Map.empty),
      meta = Map.empty,
      parameterMeta = Map.empty,
      outputs = List(OutputDefinition("procs", WdlFileType, null)),
      inputs = List.empty
    )

    val cgrepInFile = RequiredInputDefinition("in_file", WdlFileType)
    val cgrepPattern = RequiredInputDefinition("pattern", WdlStringType)
    
    val taskDefinition_cgrep = TaskDefinition(
      name = "cgrep",
      commandTemplate = null,
      runtimeAttributes = RuntimeAttributes(attributes = Map.empty),
      meta = Map.empty,
      parameterMeta = Map.empty,
      outputs = List(OutputDefinition("count", WdlIntegerType, null)),
      inputs = List(cgrepPattern, cgrepInFile)
    )

    val wcInFile = RequiredInputDefinition("in_file", WdlFileType)
    val taskDefinition_wc = TaskDefinition(
      name = "wc",
      commandTemplate = null,
      runtimeAttributes = RuntimeAttributes(attributes = Map.empty),
      meta = Map.empty,
      parameterMeta = Map.empty,
      outputs = List(OutputDefinition("count", WdlIntegerType, null)),
      inputs = List(wcInFile)
    )
    
    val workflowInputNode = RequiredGraphInputNode("cgrep.pattern", WdlStringType)
    
    val psNodeBuilder = new CallNodeBuilder()
    
    val CallNodeAndNewNodes(psCall, psGraphInputs, _) = psNodeBuilder.build("ps", taskDefinition_ps, InputDefinitionFold())
    val ps_procsOutputPort = psCall.outputByName("procs").getOrElse(fail("Unexpectedly unable to find 'ps.procs' output"))
    
    val cgrepNodeBuilder = new CallNodeBuilder()
    val cgrepInputDefinitionFold = InputDefinitionFold(
      mappings = Map(
        cgrepPattern -> Coproduct[InputDefinitionPointer](workflowInputNode.singleOutputPort: OutputPort),
        cgrepInFile -> Coproduct[InputDefinitionPointer](psCall.outputPorts.head: OutputPort)
      ),
      Set(
        cgrepNodeBuilder.makeInputPort(cgrepInFile, psCall.outputPorts.head: OutputPort),
        cgrepNodeBuilder.makeInputPort(cgrepPattern, workflowInputNode.singleOutputPort: OutputPort)
      ),
      Set(workflowInputNode)
    )
    val CallNodeAndNewNodes(cgrepCall, cgrepGraphInputs, _) = cgrepNodeBuilder.build("cgrep", taskDefinition_cgrep, cgrepInputDefinitionFold)
    val cgrep_countOutputPort = cgrepCall.outputByName("count").getOrElse(fail("Unexpectedly unable to find 'cgrep.count' output"))

    val wcNodeBuilder = new CallNodeBuilder()
    val wcInputDefinitionFold = InputDefinitionFold(
      mappings = Map(
        wcInFile -> Coproduct[InputDefinitionPointer](psCall.outputPorts.head)
      ),
      Set(
        wcNodeBuilder.makeInputPort(cgrepInFile, psCall.outputPorts.head)
      ),
      Set.empty
    )
    
    val CallNodeAndNewNodes(wcCall, wcGraphInputs, _) = wcNodeBuilder.build("wc", taskDefinition_wc, wcInputDefinitionFold)
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
    val threeStepGraph = makeThreeStep
    val threeStepWorkflow = WorkflowDefinition("three_step", threeStepGraph, Map.empty, Map.empty, List.empty)
    val threeStepNodeBuilder = new CallNodeBuilder()

    val workflowInputNode = RequiredGraphInputNode("three_step.cgrep.pattern", WdlStringType)
    
    val inputDefinitionFold = InputDefinitionFold(
      mappings = Map.empty,
      Set.empty,
      Set(workflowInputNode)
    )
    val CallNodeAndNewNodes(threeStepCall, threeStepInputs, _) = threeStepNodeBuilder.build("three_step", threeStepWorkflow, inputDefinitionFold)

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

  it should "validate workflow inputs and produce correct mapping" in {
    val requiredGin = RequiredGraphInputNode("required", WdlStringType)
    val defaultExpression = PlaceholderWomExpression(Set.empty, WdlStringType)
    val optionalWithDefaultGin = OptionalGraphInputNodeWithDefault("optional_with_default", WdlStringType, defaultExpression)
    val optionalGin = OptionalGraphInputNode("optional", WdlOptionalType(WdlStringType))
    val gins: Set[GraphNode] = Set(requiredGin, optionalWithDefaultGin, optionalGin)

    val graph = Graph.validateAndConstruct(gins).getOrElse(fail("Failed to validate graph"))

    // Missing required input
    val missingRequiredInput = graph.validateWorkflowInputs(Map.empty)
    missingRequiredInput shouldBe "Cannot find an input value for required".invalidNel

    // With only required input
    val withOnlyRequiredValue = graph.validateWorkflowInputs(Map(
      "required" -> WdlString("hello")
    ))
    withOnlyRequiredValue shouldBe Map(
      requiredGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlString("hello"): WdlValue),
      optionalWithDefaultGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](defaultExpression: WomExpression),
      optionalGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlOptionalValue.none(WdlStringType): WdlValue)
    ).validNel

    // With required input and optionalWithDefault input
    val withRequiredAndOptional = graph.validateWorkflowInputs(Map(
      "required" -> WdlString("hello"),
      "optional_with_default" -> WdlString("hola"),
      "optional" -> WdlString("ciao")
    ))

    withRequiredAndOptional shouldBe Map(
      requiredGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlString("hello"): WdlValue),
      optionalWithDefaultGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlString("hola"): WdlValue),
      optionalGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlOptionalValue(WdlString("ciao")): WdlValue)
    ).validNel
  }
}
