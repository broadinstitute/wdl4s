package wdl4s.wom.executable

import ExecutableInputValidation._
import cats.syntax.validated._
import lenthall.Checked
import lenthall.validation.ErrorOr._
import shapeless.Coproduct
import wdl4s.wdl.types.WdlType
import wdl4s.wdl.values.WdlValue
import wdl4s.wom.callable.Callable
import wdl4s.wom.executable.Executable.{DelayedCoercionFunction, InputParsingFunction, ResolvedExecutableInputs}
import wdl4s.wom.graph.Graph.ResolvedExecutableInput
import wdl4s.wom.graph.GraphNodePort.OutputPort
import wdl4s.wom.graph._

object Executable {

  /*
    * Function provided by each language, that takes the raw input file as a String and returns a Checked[ParsedInputMap]
    * Each entry of the map is an input found in the file.
    * The key is a string representation of the input. It must be be equal to the name of the matching GraphInputNode.
    * The value is a function which given a WdlType, attempts to coerce the input value to that type.
    * Thanks to this level of indirection, the logic that links graph input nodes to input values still resides in WOM,
    * which 1) abstracts it away and 2) guarantees that the linking mechanism is the same regardless of the language.
    * At the same time each language can parse the input file however it wants.
   */
  type InputParsingFunction = String => Checked[ParsedInputMap]
  type ParsedInputMap = Map[String, DelayedCoercionFunction]
  type DelayedCoercionFunction = WdlType => ErrorOr[WdlValue]
  
  /*
    * Maps output ports from graph input nodes to ResolvedExecutableInput
   */
  type ResolvedExecutableInputs = Map[OutputPort, ResolvedExecutableInput]
}

/**
  * Closely related to the WdlNamespace, contains a set of Workflows and Tasks with a single Callable selected as the
  * entry point.
  * @param entryPoint callable that this executable wraps
  * @param inputParsingFunction function that can parse an input file content and return a Checked[ParsedInputMap]
  */
final case class Executable(entryPoint: Callable, inputParsingFunction: InputParsingFunction) {
  val graph: ErrorOr[Graph] = entryPoint.graph

  /**
    * Takes in the raw input file for the executable, validates it, and returns either a validation error or a
    * Map[OutputPort, ResolvedExecutableInput] where keys are output ports from the ExternalGraphInputNodes of this executable,
    * and keys are either WdlValues or WomExpressions.
    * @param inputFile content of the input file as a string
    * @return Resolved inputs map
    */
  def validateExecutableInputs(inputFile: String): Checked[ResolvedExecutableInputs] = {
    validateInputs(graph, inputParsingFunction, parseGraphInputs, inputFile)
  }

  /**
    * Given the graph and the Map[String, DelayedCoercionFunction], attempts to find a value in the map for each ExternalGraphInputNode of the graph
    */
  private def parseGraphInputs(graph: Graph, inputCoercionMap: Map[String, DelayedCoercionFunction]): ErrorOr[Map[OutputPort, ResolvedExecutableInput]] = {
    def fromInputMapping(gin: ExternalGraphInputNode): Option[ErrorOr[ResolvedExecutableInput]] = {
      inputCoercionMap.get(gin.fullyQualifiedIdentifier).map(_(gin.womType).map(Coproduct[ResolvedExecutableInput](_)))
    }

    def fallBack(gin: ExternalGraphInputNode): ErrorOr[ResolvedExecutableInput] = gin match {
      case required: RequiredGraphInputNode => s"Required workflow input '${required.fullyQualifiedIdentifier}' not specified".invalidNel
      case optionalWithDefault: OptionalGraphInputNodeWithDefault => Coproduct[ResolvedExecutableInput](optionalWithDefault.default).validNel
      case optional: OptionalGraphInputNode => Coproduct[ResolvedExecutableInput](optional.womType.none: WdlValue).validNel
    }

    graph.inputNodes.collect({
      case gin: ExternalGraphInputNode =>
        // The compiler needs the type ascription for some reason
        (gin.singleOutputPort: OutputPort) -> fromInputMapping(gin).getOrElse(fallBack(gin))
    }).toMap.sequence
  }
}
