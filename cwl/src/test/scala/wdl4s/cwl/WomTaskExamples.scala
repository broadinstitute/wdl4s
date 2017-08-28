package wdl4s.cwl

import wdl4s.wdl.{RuntimeAttributes, WdlExpression}
import wdl4s.wdl.command.{CommandPart, StringCommandPart}
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.WomExpression

object WomTaskExamples {

  private def scp(literal: String) = StringCommandPart(literal)

  private def newTask(name: String, commandTemplate: Seq[CommandPart]) =
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

  val helloWorldLiterally : TaskDefinition = newTask("hello", Seq(scp("echo Hello World")))

}
