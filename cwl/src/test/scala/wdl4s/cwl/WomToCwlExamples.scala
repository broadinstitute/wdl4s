package wdl4s.cwl

import shapeless.Coproduct
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand, StringOrExpression}
import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.WomExpression

object WomToCwlExamples {

  private def scp(literal: String) = StringCommandPart(literal)

  private def pcp(expressionString: String) = {
    val attributes = Map.empty[String, String]
    val expression = WdlExpression.fromString(expressionString)
    ParameterCommandPart(attributes, expression)
  }

  private def womTask(name: String, commandTemplate: Seq[CommandPart]) =
    TaskDefinition(
      name = name,
      commandTemplate = commandTemplate,
      runtimeAttributes = RuntimeAttributes(Map.empty[String, WdlExpression]),
      meta = Map.empty[String, String],
      parameterMeta = Map.empty[String, String],
      outputs = Set.empty[Callable.OutputDefinition],
      inputs = Set.empty[Callable.InputDefinition],
      declarations = List.empty[(String, WomExpression)]
    )

  private def bind(pos: Int, value:String): CommandLineBinding = {
    CommandLineBinding(
      position = Option(pos),
      valueFrom = Option(Coproduct[StringOrExpression](value))
    )
  }

  private def cwlCmd(base: String)(args: CommandLineBinding*)(ins: (String, CommandLineBinding)*)
  : CommandLineTool = {
    val baseCommand = Option(Coproduct[BaseCommand](base))
    val arguments = Option(args.map(Coproduct[Argument](_)).toArray)
    val inputs = ins.map {
      case (id: String, binding: CommandLineBinding) =>
        CommandInputParameter(
          id = id,
          inputBinding = Option(binding)
        )
    }.toArray
    CommandLineTool(
      baseCommand = baseCommand,
      arguments = arguments,
      inputs = inputs
    )
  }

  case class Example(womTask: TaskDefinition, cwlCmdExpected: CommandLineTool)

  val helloWorldLiterals: Example = Example(
    womTask("hello", Seq(scp("echo Hello World"))),
    cwlCmd("echo")(bind(2, "Hello"), bind(3, "World"))()
  )

  val helloWorldOneParam: Example = Example(
    womTask("hello", Seq(scp("echo "), pcp("greeting"))),
    cwlCmd("echo")()()
  )

  val helloWorldLiterally: TaskDefinition = womTask("hello", Seq(scp("echo Hello World")))

  val examples: Seq[Example] = Seq(helloWorldLiterals)
}
