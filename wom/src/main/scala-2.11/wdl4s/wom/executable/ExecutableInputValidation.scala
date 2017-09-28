package wdl4s.wom.executable

import cats.syntax.either._
import lenthall.Checked
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.wom.executable.Executable.{DelayedCoercionFunction, InputParsingFunction, ResolvedExecutableInputs}
import wdl4s.wom.graph.Graph

private [executable] object ExecutableInputValidation {
  /**
    * Takes in the raw input file for the executable, validates it, and returns either a validation error or a
    * Map[OutputPort, ResolvedExecutableInput] where keys are output ports from the ExternalGraphInputNodes of this executable,
    * and keys are either WdlValues or WomExpressions.
    * @param inputFile content of the input file as a string
    * @return Resolved inputs map
    */
  private [executable] def validateInputs(graph: ErrorOr[Graph],
                             inputParsingFunction: InputParsingFunction,
                             parseGraphInputs: (Graph, Map[String, DelayedCoercionFunction]) => ErrorOr[ResolvedExecutableInputs],
                             inputFile: String): Checked[ResolvedExecutableInputs] = for {
    validGraph <- graph.toEither
    parsedInputs <- inputParsingFunction(inputFile)
    validatedInputs <- parseGraphInputs(validGraph, parsedInputs).toEither
  } yield validatedInputs
}
