package wdl4s.wom.callable

import wdl4s.wdl._
import wdl4s.wdl.command.CommandPart
import wdl4s.wdl.expression.WdlFunctions
import wdl4s.wdl.util.StringUtil
import wdl4s.wdl.values.{WdlFile, WdlValue}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

case class TaskDefinition(name: String,
                          commandTemplate: Seq[CommandPart],
                          runtimeAttributes: RuntimeAttributes,
                          meta: Map[String, String],
                          parameterMeta: Map[String, String],
                          outputs: Set[Callable.OutputDefinition],
                          inputs: Set[_ >: Callable.InputDefinition]) extends Callable {

  val unqualifiedName: LocallyQualifiedName = name

  def lookupFunction(knownInputs: WorkflowCoercedInputs,
                     wdlFunctions: WdlFunctions[WdlValue],
                     outputResolver: OutputResolver = NoOutputResolver,
                     shards: Map[Scatter, Int] = Map.empty[Scatter, Int],
                     relativeTo: Scope = ???): String => WdlValue = ???

  def instantiateCommand(taskInputs: EvaluatedTaskInputs,
                         functions: WdlFunctions[WdlValue],
                         valueMapper: WdlValue => WdlValue = (v) => v): Try[String] = {
    // TODO: Bring back inputs: Try(StringUtil.normalize(commandTemplate.map(_.instantiate(declarations, taskInputs, functions, valueMapper)).mkString("")))
    Try(StringUtil.normalize(commandTemplate.map(_.instantiate(Seq.empty, taskInputs, functions, valueMapper)).mkString("")))
  }

  def commandTemplateString: String = StringUtil.normalize(commandTemplate.map(_.toString).mkString)

  override def toString: String = s"[Task name=$name commandTemplate=$commandTemplate}]"

  /**
    * A lookup function that restricts known task values to input declarations
    */
  def inputsLookupFunction(inputs: WorkflowCoercedInputs,
                           wdlFunctions: WdlFunctions[WdlValue],
                           shards: Map[Scatter, Int] = Map.empty[Scatter, Int]): String => WdlValue = ???

  def evaluateFilesFromCommand(inputs: WorkflowCoercedInputs,
                               functions: WdlFunctions[WdlValue]) = ???

  /**
    * Tries to determine files that will be created by the outputs of this task.
    */
  def findOutputFiles(inputs: WorkflowCoercedInputs,
                      functions: WdlFunctions[WdlValue],
                      silenceEvaluationErrors: Boolean = true): Seq[WdlFile] = {
    val outputFiles = outputs.toList flatMap { taskOutput =>
      val lookup = NoLookup // TODO: Reinstate: val lookup = lookupFunction(inputs, functions, relativeTo = taskOutput)
      taskOutput.expression.evaluateFiles(lookup, functions, taskOutput.womType) match {
        case Success(wdlFiles) => wdlFiles
        case Failure(ex) => if (silenceEvaluationErrors) Seq.empty[WdlFile] else throw ex
      }
    }

    outputFiles.distinct
  }

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
