package wdl4s.wom

import lenthall.util.TryUtil
import wdl4s.wdl._
import wdl4s.wdl.command.{CommandPart, ParameterCommandPart}
import wdl4s.wdl.expression.{WdlFunctions, WdlStandardLibraryFunctions}
import wdl4s.wdl.util.StringUtil
import wdl4s.wdl.values.{WdlFile, WdlValue}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object CommonTask {

  def apply(task: Task): CommonTask = {

    CommonTask(task.name, task.commandTemplate, task.runtimeAttributes, task.meta,
      task.parameterMeta, task.writeExpressions, task.outputs, task.declarations, lookupFunction = ???)
  }

}

case class CommonTask(name: String,
                      commandTemplate: Seq[CommandPart],
                      runtimeAttributes: RuntimeAttributes,
                      meta: Map[String, String],
                      parameterMeta: Map[String, String],
                      writeExpressions: Seq[WdlExpression],
                      taskOutputs: Seq[TaskOutput],
                      declarations: Seq[Declaration],
                      lookupFunction: String => WdlValue) {

  val unqualifiedName = name

  val outputs: Seq[TaskOutput] = taskOutputs


  def instantiateCommand(taskInputs: EvaluatedTaskInputs,
                         functions: WdlFunctions[WdlValue],
                         valueMapper: WdlValue => WdlValue = (v) => v): Try[String] = {
    Try(StringUtil.normalize(commandTemplate.map(_.instantiate(declarations, taskInputs, functions, valueMapper)).mkString("")))
  }

  def commandTemplateString: String = StringUtil.normalize(commandTemplate.map(_.toString).mkString)

  override def toString: String = s"[Task name=$name commandTemplate=$commandTemplate}]"

  /**
    * A lookup function that restricts known task values to input declarations
    */
  def inputsLookupFunction(inputs: WorkflowCoercedInputs,
                           wdlFunctions: WdlFunctions[WdlValue],
                           shards: Map[Scatter, Int] = Map.empty[Scatter, Int]): String => WdlValue = {
    outputs.toList match {
      case head :: others => super.lookupFunction(inputs, wdlFunctions, NoOutputResolver, shards, relativeTo = head)
      case _ => super.lookupFunction(inputs, wdlFunctions, NoOutputResolver, shards, this)
    }
  }

  def evaluateFilesFromCommand(inputs: WorkflowCoercedInputs,
                               functions: WdlFunctions[WdlValue]) = {
    val commandExpressions = commandTemplate.collect({
      case x: ParameterCommandPart => x.expression
    })

    val lookup = lookupFunction(inputs, functions)
    val evaluatedExpressionMap = writeExpressions map { e => e -> e.evaluate(lookup, functions) } toMap

    evaluatedExpressionMap collect {
      case (k, Success(file: WdlFile)) => k -> file
    }
  }

  /**
    * Tries to determine files that will be created by the outputs of this task.
    */
  def findOutputFiles(inputs: WorkflowCoercedInputs,
                      functions: WdlFunctions[WdlValue],
                      silenceEvaluationErrors: Boolean = true): Seq[WdlFile] = {
    val outputFiles = outputs flatMap { taskOutput =>
      val lookup = lookupFunction(inputs, functions, relativeTo = taskOutput)
      taskOutput.requiredExpression.evaluateFiles(lookup, functions, taskOutput.wdlType) match {
        case Success(wdlFiles) => wdlFiles
        case Failure(ex) => if (silenceEvaluationErrors) Seq.empty[WdlFile] else throw ex
      }
    }

    outputFiles.distinct
  }

  def evaluateOutputs(inputs: EvaluatedTaskInputs,
                      wdlFunctions: WdlStandardLibraryFunctions,
                      postMapper: WdlValue => Try[WdlValue] = v => Success(v)): Try[Map[TaskOutput, WdlValue]] = {
    val fqnInputs = inputs map { case (d, v) => d.fullyQualifiedName -> v }
    val evaluatedOutputs = outputs.foldLeft(Map.empty[TaskOutput, Try[WdlValue]])((outputMap, output) => {
      val currentOutputs = outputMap collect {
        case (outputName, value) if value.isSuccess => outputName.fullyQualifiedName -> value.get
      }
      def knownValues = currentOutputs ++ fqnInputs
      val lookup = lookupFunction(knownValues, wdlFunctions, relativeTo = output)
      val coerced = output.requiredExpression.evaluate(lookup, wdlFunctions) flatMap output.wdlType.coerceRawValue
      val jobOutput = output -> (coerced flatMap postMapper).recoverWith {
        case t: Throwable => Failure(new RuntimeException(s"Could not evaluate ${output.fullyQualifiedName} = ${output.requiredExpression.toWdlString}", t))
      }
      outputMap + jobOutput
    }) map { case (k, v) => k -> v }

    TryUtil.sequenceMap(evaluatedOutputs, "Failed to evaluate outputs.")
  }

  /**
    * Assign declaration values from the given input map.
    * Fqn must be task declaration fqns
    * e.g.:
    * task t {
    *   String s
    * }
    * inputMap = Map("t.s" -> WdlString("hello"))
    */
  def inputsFromMap(inputs: Map[FullyQualifiedName, WdlValue]): EvaluatedTaskInputs = {
    declarations flatMap { declaration =>
      inputs collectFirst {
        case (fqn, value) if fqn == declaration.fullyQualifiedName => declaration -> value }
    } toMap
  }


}