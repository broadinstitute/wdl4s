package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.cwl.CromwomificationWIPPlaceholders.AstNodeForLiteralWdlExpression
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.wdl.expression.PureStandardLibraryFunctionsLike
import wdl4s.wdl.values.WdlValue
import wdl4s.wdl.{Declaration, EvaluatedTaskInputs, RuntimeAttributes, WdlExpression}
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.WomExpression

class ConvertCromWomToCwlSpec extends FlatSpec with Matchers {

  it should "convert CromWOM TaskDefinition to CWL CommandLineTool" in {
    val taskDef = TaskDefinition(
      name = "message",
      commandTemplate = Seq(
        StringCommandPart("echo "),
        ParameterCommandPart(
          attributes = Map.empty[String, String],
          expression = WdlExpression(
            ast = AstNodeForLiteralWdlExpression("Hello")
          )
        )
      ): Seq[CommandPart],
      runtimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression]),
      meta = Map.empty[String, String],
      parameterMeta = Map.empty[String, String],
      outputs = Set.empty[Callable.OutputDefinition],
      inputs = Set.empty[Callable.InputDefinition],
      declarations = List.empty[(String, WomExpression)]
    )
    val taskInputs: EvaluatedTaskInputs = Map.empty[Declaration, WdlValue]
    val functions = new PureStandardLibraryFunctionsLike {}
    val valueMapper: WdlValue => WdlValue = (value: WdlValue) => value
    val taskString = taskDef.instantiateCommand(taskInputs, functions, valueMapper).get
    println(taskString)
    taskString.contains("echo") shouldBe true
  }

}
