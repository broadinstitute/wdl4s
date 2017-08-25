package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.parser.WdlParser.Terminal
import wdl4s.wdl.command.{ParameterCommandPart, StringCommandPart}
import wdl4s.wdl.expression.PureStandardLibraryFunctionsLike
import wdl4s.wdl.values.WdlValue
import wdl4s.wdl.{Declaration, RuntimeAttributes, WdlExpression}
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.WomExpression
class WomToCwlSpec extends FlatSpec with Matchers {

  it should "convert command line" in {
    val parser = WomToCwl.parser
    val commandLine = Seq(StringCommandPart("echo Hello World"))
    println(parser.tokenize(parser.scan(commandLine)))
    val baseCommand = WomToCwl.toBaseCommand(WomToCwl.parser.tokenize(WomToCwl.parser.scan(commandLine)))
    baseCommand.toOption.get.select[String].get shouldBe "echo"
  }

  it should "convert CromWOM TaskDefinition to CWL CommandLineTool" in {
    val mockAstId = 42
    val mockLine = 0
    val mockCol = 0
    val taskDef = TaskDefinition(
      name = "message",
      commandTemplate = Seq(
        StringCommandPart("echo "),
        ParameterCommandPart(
          attributes = Map.empty[String, String],
          expression = WdlExpression(
            ast = new Terminal(mockAstId, "string", "Hello", "Yo", mockLine, mockCol)
          )
        )
      ),
      runtimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression]),
      meta = Map.empty[String, String],
      parameterMeta = Map.empty[String, String],
      outputs = Set.empty[Callable.OutputDefinition],
      inputs = Set.empty[Callable.InputDefinition],
      declarations = List.empty[(String, WomExpression)]
    )
    val taskInputs = Map.empty[Declaration, WdlValue]
    val functions = new PureStandardLibraryFunctionsLike {}
    val valueMapper = (value: WdlValue) => value
    val taskString = taskDef.instantiateCommand(taskInputs, functions, valueMapper).get
    println(taskString)
    taskString shouldBe "echo Hello"
  }

}
