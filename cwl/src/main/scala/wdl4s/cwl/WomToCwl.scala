package wdl4s.cwl

import cats.Apply
import cats.data.Validated.Valid
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.cwl.CommandLineTool.BaseCommand
import wdl4s.wom.callable.TaskDefinition

object WomToCwl {

  def toBaseCommand(task: TaskDefinition): ErrorOr[BaseCommand] = ???

  def placeholder: ErrorOr[String] = Valid("Yo")

  def toCwl(task: TaskDefinition): ErrorOr[CommandLineTool] =
    Apply[ErrorOr[CommandLineTool]].map2(
      toBaseCommand(task),
      placeholder
    )(
      (baseCommand, placeHolder) =>
        CommandLineTool(
          baseCommand = Option(baseCommand)
        )
    )

}
