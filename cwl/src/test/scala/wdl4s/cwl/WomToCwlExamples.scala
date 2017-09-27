package wdl4s.cwl

import shapeless.Coproduct
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand, StringOrExpression}
import wdl4s.wdl.WdlExpression
import wdl4s.wdl.command.{ParameterCommandPart, StringCommandPart}
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.{CommandPart, RuntimeAttributes}

object WomToCwlExamples {

  private def scp(literal: String) = StringCommandPart(literal)

  private def pcp(expressionString: String) = {
    val attributes = Map.empty[String, String]
    val expression = WdlExpression.fromString(expressionString)
    ParameterCommandPart(attributes, expression)
  }

  private def womTask(name: String)(commandTemplate: CommandPart*) =
    TaskDefinition(
      name = name,
      commandTemplate = commandTemplate,
      runtimeAttributes = RuntimeAttributes(Map.empty[String, WomExpression]),
      meta = Map.empty[String, String],
      parameterMeta = Map.empty[String, String],
      outputs = List.empty[Callable.OutputDefinition],
      inputs = List.empty[Callable.InputDefinition]
    )

  private def bind(pos: Int, value: String): CommandLineBinding = {
    CommandLineBinding(
      position = Option(pos),
      valueFrom = Option(Coproduct[StringOrExpression](value))
    )
  }

  private case class Input(pos: Int, prefixOption: Option[String], separate: Boolean, value: String) {
    val id = s"input$pos"
  }


  private def cwlCmd(base: String)(args: CommandLineBinding*)(ins: Input*)
  : CommandLineTool = {
    val baseCommand = Option(Coproduct[BaseCommand](base))
    val arguments = Option(args.map(Coproduct[Argument](_)).toArray)
    val inputs = ins.map { input =>
      val binding = CommandLineBinding(
        position = Option(input.pos),
        prefix = input.prefixOption,
        separate = Option(if (input.separate) "true" else "false"),
        valueFrom = Option(Coproduct[StringOrExpression](input.value))
      )
      CommandInputParameter(
        id = input.id,
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
    womTask("hello")(scp("echo Hello World")),
    cwlCmd("echo")(bind(0, "Hello"), bind(1, "World"))()
  )

  val helloWorldParam: Example = Example(
    womTask("hello")(scp("echo "), pcp("greeting")),
    cwlCmd("echo")()(Input(0, None, separate = false, "greeting"))
  )

  val concat: Example = Example(
    womTask("cat")(scp("cat "), pcp("in1"), scp(" "), pcp("in2"), scp(" "), pcp("out")),
    cwlCmd("cat")()(
      Input(0, None, separate = false, "in1"),
      Input(1, None, separate = false, "in2"),
      Input(2, None, separate = false, "out")
    )
  )

  val dd: Example = Example(
    womTask("dd")(scp("dd in="), pcp("infile"), scp(" out="), pcp("outfile")),
    cwlCmd("dd")()(
      Input(0, Option("in="), separate = false, "infile"),
      Input(1, Option("out="), separate = false, "outfile")
    )
  )

  val clayc: Example = Example(
    womTask("clayc")(scp("clayc -v -f -i "), pcp("infile"), scp(" -o "), pcp("outfile")),
    cwlCmd("clayc")(bind(0, "-v"), bind(1, "-f"))(
      Input(3, Some("-i"), separate = true, "infile"),
      Input(5, Some("-o"), separate = true, "outfile")
    )
  )

  val examples: Seq[Example] = Seq(helloWorldLiterals, helloWorldParam, concat, dd, clayc)
}
