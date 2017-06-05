package wdl4s.wom.callable

import lenthall.util.TryUtil
import wdl4s.wdl.{NoLookup, WdlExpression}
import wdl4s.wdl.expression.WdlFunctions
import wdl4s.wdl.types.WdlType
import wdl4s.wdl.values.WdlValue
import wdl4s.wom.callable.Callable._

import scala.util.Try

trait Callable {
  def name: String
  def inputs: Set[_ >: InputDefinition]
  def outputs: Set[OutputDefinition]

  // TODO: This function probably doesn't belong here... I dunno, but just feels a bit backwards to me for a definition to be evaluating outputs?
  def evaluateOutputs(//knownInputs: WorkflowCoercedInputs,
                      wdlFunctions: WdlFunctions[WdlValue]
                      //outputResolver: OutputResolver = NoOutputResolver,
                      //shards: Map[Scatter, Int] = Map.empty[Scatter, Int]
                     ): Try[Map[Callable.OutputDefinition, WdlValue]] = {

    def evaluateOutput(outputMap: Map[Callable.OutputDefinition, Try[WdlValue]], outputDefinition: Callable.OutputDefinition): Map[Callable.OutputDefinition, Try[WdlValue]] = {
      //      val currentOutputs = outputMap collect {
      //        case (outputName, value) if value.isSuccess => outputName.fullyQualifiedName -> value.get
      //      }
      //def knownValues = currentOutputs ++ knownInputs
      val lookup = NoLookup //lookupFunction(knownValues, wdlFunctions, outputResolver, shards, output)
      val coerced = outputDefinition.expression.evaluate(lookup, wdlFunctions) flatMap outputDefinition.womType.coerceRawValue
      val workflowOutput = outputDefinition -> coerced

      outputMap + workflowOutput
    }

    val evaluatedOutputs = outputs.foldLeft(Map.empty[Callable.OutputDefinition, Try[WdlValue]])(evaluateOutput)

    TryUtil.sequenceMap(evaluatedOutputs, "Failed to evaluate workflow outputs.\n")
  }
}

object Callable {
  sealed trait InputDefinition {
    def name: String
    def womType: WdlType
  }
  final case class DeclaredInputDefinition(name: String, womType: WdlType, expression: WdlExpression)
  final case class RequiredInputDefinition(name: String, womType: WdlType)
  // Might be another input definition type, InputDefinitionWithDefault
  case class OutputDefinition(name: String, womType: WdlType, expression: WdlExpression)
}
