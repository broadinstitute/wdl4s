package wdl4s.wom

import org.scalatest.{FlatSpec, Matchers}

class ExecutableSpec extends FlatSpec with Matchers { 
  behavior of "Executable"

//  it should "validate workflow inputs and produce correct mapping" in {
//    val requiredGin = RequiredGraphInputNode("required", WdlStringType)
//    val defaultExpression = PlaceholderWomExpression(Set.empty, WdlStringType)
//    val optionalWithDefaultGin = OptionalGraphInputNodeWithDefault("optional_with_default", WdlStringType, defaultExpression)
//    val optionalGin = OptionalGraphInputNode("optional", WdlOptionalType(WdlStringType))
//    val gins: Set[GraphNode] = Set(requiredGin, optionalWithDefaultGin, optionalGin)
//
//    val graph = Graph.validateAndConstruct(gins).getOrElse(fail("Failed to validate graph"))
//    val executable
//
//    // Missing required input
//    val missingRequiredInput = graph.validateWorkflowInputs(Map.empty)
//    missingRequiredInput shouldBe "Cannot find an input value for required".invalidNel
//
//    // With only required input
//    val withOnlyRequiredValue = graph.validateWorkflowInputs(Map(
//      "required" -> WdlString("hello")
//    ))
//    withOnlyRequiredValue shouldBe Map(
//      requiredGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlString("hello"): WdlValue),
//      optionalWithDefaultGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](defaultExpression: WomExpression),
//      optionalGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlOptionalValue.none(WdlStringType): WdlValue)
//    ).validNel
//
//    // With required input and optionalWithDefault input
//    val withRequiredAndOptional = graph.validateWorkflowInputs(Map(
//      "required" -> WdlString("hello"),
//      "optional_with_default" -> WdlString("hola"),
//      "optional" -> WdlString("ciao")
//    ))
//
//    withRequiredAndOptional shouldBe Map(
//      requiredGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlString("hello"): WdlValue),
//      optionalWithDefaultGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlString("hola"): WdlValue),
//      optionalGin.singleOutputPort -> Coproduct[ResolvedWorkflowInput](WdlOptionalValue(WdlString("ciao")): WdlValue)
//    ).validNel
//  }
}
