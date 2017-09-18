package wdl4s.wom.graph

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.types.{WdlOptionalType, WdlStringType}
import wdl4s.wom.RuntimeAttributes
import wdl4s.wom.callable.Callable.{DeclaredInputDefinition, OptionalInputDefinition, OptionalInputDefinitionWithDefault, RequiredInputDefinition}
import wdl4s.wom.callable.TaskDefinition
import wdl4s.wom.expression.{PlaceholderWomExpression, WomExpression}
import wdl4s.wom.graph.GraphNodePort.{GraphNodeOutputPort, OutputPort}

class CallNodeSpec extends FlatSpec with Matchers {
  behavior of "CallNode"

  def ginNotFound(name: String) = fail(s"Can't find a GraphInputNode for $name")

  it should "produce correct task definition mappings" in {
    val requiredSatisfiedByPort = RequiredInputDefinition("required_satisfied_by_port", WdlStringType)
    val requiredSatisfiedByExpression = RequiredInputDefinition("required_satisfied_by_expression", WdlStringType)
    val requiredUnsatisfied = RequiredInputDefinition("required_unsatisfied", WdlStringType)

    val optionalWithDefaultSatisfiedByPort = OptionalInputDefinitionWithDefault("optional_with_default_satisfied_by_port", WdlStringType, PlaceholderWomExpression(Set.empty, WdlStringType))
    val optionalWithDefaultSatisfiedByExpression = OptionalInputDefinitionWithDefault("optional_with_default_satisfied_by_expression", WdlStringType, PlaceholderWomExpression(Set.empty, WdlStringType))
    val optionalWithDefaultUnsatisfied = OptionalInputDefinitionWithDefault("optional_with_default_unsatisfied", WdlStringType, PlaceholderWomExpression(Set.empty, WdlStringType))

    val optionalSatisfiedByPort = OptionalInputDefinition("optional_satisfied_by_port", WdlOptionalType(WdlStringType))
    val optionalSatisfiedByExpression = OptionalInputDefinition("optional_satisfied_by_expression", WdlOptionalType(WdlStringType))
    val optionalUnsatisfied = OptionalInputDefinition("optional_unsatisfied", WdlOptionalType(WdlStringType))

    val declarationSatisfiedByPort = DeclaredInputDefinition("declaration_satisfied_by_port", WdlStringType, PlaceholderWomExpression(Set.empty, WdlStringType))
    val declarationSatisfiedByExpression = DeclaredInputDefinition("declaration_satisfied_by_expression", WdlStringType, PlaceholderWomExpression(Set.empty, WdlStringType))
    val declarationUnsatisfied = DeclaredInputDefinition("declaration_unsatisfied", WdlStringType, PlaceholderWomExpression(Set.empty, WdlStringType))

    val taskDefinition = TaskDefinition(
      name = "td",
      commandTemplate = Seq.empty,
      runtimeAttributes = RuntimeAttributes.empty,
      meta = Map.empty,
      parameterMeta = Map.empty,
      outputs = List.empty,
      inputs = List(
        requiredSatisfiedByPort, requiredSatisfiedByExpression, requiredUnsatisfied,
        optionalWithDefaultSatisfiedByPort, optionalWithDefaultSatisfiedByExpression, optionalWithDefaultUnsatisfied,
        optionalSatisfiedByPort, optionalSatisfiedByExpression, optionalUnsatisfied,
        declarationSatisfiedByPort, declarationSatisfiedByExpression, declarationUnsatisfied
      )
    )

    val requiredPort = GraphNodeOutputPort("required_port", WdlStringType, null)
    val optionalPort = GraphNodeOutputPort("optional_port", WdlStringType, null)
    val optionalWithDefaultPort = GraphNodeOutputPort("optional_with_default_port", WdlStringType, null)
    val declarationPort = GraphNodeOutputPort("declaration_port", WdlStringType, null)

    val portInputs = Map(
      "required_satisfied_by_port" -> requiredPort,
      "optional_with_default_satisfied_by_port" -> optionalWithDefaultPort,
      "optional_satisfied_by_port" -> optionalPort,
      "declaration_satisfied_by_port" -> declarationPort
    )

    val requiredExpression = GraphNodeInputExpression("required_satisfied_by_expression", PlaceholderWomExpression(Set.empty, WdlStringType), Map("a.out" -> GraphNodeOutputPort("a", WdlStringType, null)))
    val optionalExpression = GraphNodeInputExpression("optional_satisfied_by_expression", PlaceholderWomExpression(Set.empty, WdlStringType), Map("a.out" -> GraphNodeOutputPort("a", WdlStringType, null)))
    val optionalWithDefaultExpression = GraphNodeInputExpression("optional_with_default_satisfied_by_expression", PlaceholderWomExpression(Set.empty, WdlStringType), Map("a.out" -> GraphNodeOutputPort("a", WdlStringType, null)))
    val declarationExpression = GraphNodeInputExpression("declaration_satisfied_by_expression", PlaceholderWomExpression(Set.empty, WdlStringType), Map("a.out" -> GraphNodeOutputPort("a", WdlStringType, null)))

    val expressionInputs = Set(requiredExpression, optionalExpression, optionalWithDefaultExpression, declarationExpression)

    val callWithInputs = CallNode.callWithInputs("call_name", taskDefinition, portInputs, expressionInputs).getOrElse(fail("Failed to create call with inputs"))

    val newInputs = callWithInputs.newInputs
    
    // One for each unsatisfied ExternalGraphInputNode (DeclaredInputDefinitions immediately fall back to their default and 
    // don't generate graph input nodes)
    newInputs should have size 3

    val requiredInput = newInputs.find(_.name == "call_name.required_unsatisfied")
      .getOrElse(ginNotFound("call_name.required_unsatisfied"))
    requiredInput.isInstanceOf[RequiredGraphInputNode] shouldBe true

    val optionalInput = newInputs.find(_.name == "call_name.optional_unsatisfied")
      .getOrElse(ginNotFound("call_name.optional_unsatisfied"))
    optionalInput
      .isInstanceOf[OptionalGraphInputNode] shouldBe true

    val optionalInputWithDefault = newInputs.find(_.name == "call_name.optional_with_default_unsatisfied")
      .getOrElse(ginNotFound("call_name.optional_with_default_unsatisfied"))
    optionalInputWithDefault
      .asInstanceOf[OptionalGraphInputNodeWithDefault].default shouldBe optionalWithDefaultUnsatisfied.default

    // Validate input mappings are correct
    val inputDefinitionMappings = callWithInputs.node.inputDefinitionMappings
    inputDefinitionMappings.size == taskDefinition.inputs.size shouldBe true

    /* *** Unsatisfied *** */
    // unsatisfied input defs should map to the above newInput output ports
    inputDefinitionMappings(requiredUnsatisfied).select[OutputPort].get eq requiredInput.singleOutputPort shouldBe true
    inputDefinitionMappings(optionalUnsatisfied).select[OutputPort].get eq optionalInput.singleOutputPort shouldBe true
    inputDefinitionMappings(optionalWithDefaultUnsatisfied).select[OutputPort].get eq optionalInputWithDefault.singleOutputPort shouldBe true
    
    // unsatisfied declared input def gets a FloatingExpressionNode
    inputDefinitionMappings(declarationUnsatisfied).select[WomExpression].get shouldBe declarationUnsatisfied.expression

    /* *** Satisfied by port *** */
    // satisfied by port should map to their respective output port from the  
    inputDefinitionMappings(requiredSatisfiedByPort).select[OutputPort].get shouldBe requiredPort
    inputDefinitionMappings(optionalSatisfiedByPort).select[OutputPort].get shouldBe optionalPort
    inputDefinitionMappings(optionalWithDefaultSatisfiedByPort).select[OutputPort].get shouldBe optionalWithDefaultPort
    inputDefinitionMappings(declarationSatisfiedByPort).select[OutputPort].get shouldBe declarationPort

    /* *** Satisfied by expression *** */
    // satisfied by expression should map to output port attached to ExpressionNodes carrying the corresponding instantiated expression
    inputDefinitionMappings(requiredSatisfiedByExpression)
      .select[InstantiatedExpression].get.expression shouldBe requiredExpression.expression
    
    inputDefinitionMappings(optionalSatisfiedByExpression)
      .select[InstantiatedExpression].get.expression shouldBe optionalExpression.expression
    
    inputDefinitionMappings(optionalWithDefaultSatisfiedByExpression)
      .select[InstantiatedExpression].get.expression shouldBe optionalWithDefaultExpression.expression
    
    inputDefinitionMappings(declarationSatisfiedByExpression)
      .select[InstantiatedExpression].get.expression shouldBe declarationExpression.expression
  }
}
