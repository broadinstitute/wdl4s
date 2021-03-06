package wom.callable

import lenthall.validation.ErrorOr.ErrorOr
import wdl._
import wdl.expression.WdlFunctions
import wdl.util.StringUtil
import wdl.values.WdlValue
import wom.expression.IoFunctionSet
import wom.graph.{Graph, TaskCall}
import wom.{CommandPart, RuntimeAttributes, WomEvaluatedCallInputs}

import scala.util.Try

case class TaskDefinition(name: String,
                          commandTemplate: Seq[CommandPart],
                          runtimeAttributes: RuntimeAttributes,
                          meta: Map[String, String],
                          parameterMeta: Map[String, String],
                          outputs: List[Callable.OutputDefinition],
                          inputs: List[_ <: Callable.InputDefinition],
                          prefixSeparator: String = ".",
                          commandPartSeparator: String = "") extends Callable {

  val unqualifiedName: LocallyQualifiedName = name

  override lazy val graph: ErrorOr[Graph] = TaskCall.graphFromDefinition(this)

  def lookupFunction(knownInputs: WorkflowCoercedInputs,
                     wdlFunctions: WdlFunctions[WdlValue],
                     outputResolver: OutputResolver = NoOutputResolver,
                     shards: Map[Scatter, Int] = Map.empty[Scatter, Int],
                     relativeTo: Scope = null): String => WdlValue = ???

  def instantiateCommand(taskInputs: WomEvaluatedCallInputs,
                         functions: IoFunctionSet,
                         valueMapper: WdlValue => WdlValue = identity[WdlValue],
                         separate: Boolean = false): Try[String] = {
    val mappedInputs = taskInputs.map({case (k, v) => k.localName -> v})
    // TODO: Bring back inputs: Try(StringUtil.normalize(commandTemplate.map(_.instantiate(declarations, taskInputs, functions, valueMapper)).mkString("")))
    Try(StringUtil.normalize(commandTemplate.map(_.instantiate(mappedInputs, functions, valueMapper)).mkString(commandPartSeparator)))
  }

  def commandTemplateString: String = StringUtil.normalize(commandTemplate.map(_.toString).mkString)

  override def toString: String = s"[Task name=$name commandTemplate=$commandTemplate}]"


  // TODO: fixup? The general version in Callable might not be good enough for Task:
//  def evaluateOutputs(inputs: EvaluatedTaskInputs,
//                      wdlFunctions: WdlStandardLibraryFunctions,
//                      postMapper: WdlValue => Try[WdlValue] = v => Success(v)): Try[Map[TaskOutput, WdlValue]] = {
//    val fqnInputs = inputs map { case (d, v) => d.fullyQualifiedName -> v }
//    val evaluatedOutputs = outputs.foldLeft(Map.empty[TaskOutput, Try[WdlValue]])((outputMap, output) => {
//      val currentOutputs = outputMap collect {
//        case (outputName, value) if value.isSuccess => outputName.fullyQualifiedName -> value.get
//      }
//      def knownValues = currentOutputs ++ fqnInputs
//      val lookup = NoLookup // TODO: Reinstate: val lookup = lookupFunction(knownValues, wdlFunctions, relativeTo = output)
//      val coerced = output.expression.evaluate(lookup, wdlFunctions) flatMap output.womType.coerceRawValue
//      val jobOutput = output -> (coerced flatMap postMapper).recoverWith {
//        case t: Throwable => Failure(new RuntimeException(s"Could not evaluate ${output.name} = ${output.expression.toWdlString}", t))
//      }
//      outputMap + jobOutput
//    }) map { case (k, v) => k -> v }
//
//    TryUtil.sequenceMap(evaluatedOutputs, "Failed to evaluate outputs.")
//  }

  /**
    * Assign declaration values from the given input map.
    * Fqn must be task declaration fqns
    * e.g.:
    * task t {
    *   String s
    * }
    * inputMap = Map("t.s" -> WdlString("hello"))
    */
  def inputsFromMap(suppliedInputs: Map[FullyQualifiedName, WdlValue]): EvaluatedTaskInputs = {
    // TODO: Reinstate:
//    inputs flatMap { i =>
//      suppliedInputs collectFirst {
//        case (fqn, value) if fqn == i.name => i -> value }
//    } toMap
    Map.empty
  }
}
