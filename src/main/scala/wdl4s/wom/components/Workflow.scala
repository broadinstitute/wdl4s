package wdl4s.wom.components

import lenthall.util.TryUtil
import wdl4s.wdl._
import wdl4s.wdl.expression.WdlFunctions
import wdl4s.wdl.values.WdlValue

import scala.language.postfixOps
import scala.util.Try

final case class Workflow(name: String,
                          inputs: Map[FullyQualifiedName, WorkflowInput],
                          outputs: Seq[WorkflowOutput],
                          calls: Set[Call],
                          meta: Map[String, String],
                          parameterMeta: Map[String, String]) {

  /**
    * Find a Call object by name.  For example,
    *
    * {{{
    * workflow w {
    *   call foobar
    * }
    * }}}
    *
    * calling findCallByName("foobar") will return a Some(call).  All
    * other values would return a None
    *
    * @param name name of call to return
    * @return Some(Call) if one with that name was found otherwise None
    */
  def findCallByName(name: String): Option[Call] = calls.find(_.unqualifiedName == name)

  def findWorkflowOutputByName(name: String, relativeTo: Scope) = {
    val leftOutputs = if (outputs.contains(relativeTo)) {
      outputs.dropRight(outputs.size - outputs.indexOf(relativeTo))
    } else { outputs }

    leftOutputs.find(_.unqualifiedName == name)
  }

  override def toString = s"[Workflow $name]"

  def evaluateOutputs(//knownInputs: WorkflowCoercedInputs,
                      wdlFunctions: WdlFunctions[WdlValue]
                      //outputResolver: OutputResolver = NoOutputResolver,
                      //shards: Map[Scatter, Int] = Map.empty[Scatter, Int]
                     ): Try[Map[WorkflowOutput, WdlValue]] = {
    
    val evaluatedOutputs = outputs.foldLeft(Map.empty[WorkflowOutput, Try[WdlValue]])((outputMap, output) => {
//      val currentOutputs = outputMap collect {
//        case (outputName, value) if value.isSuccess => outputName.fullyQualifiedName -> value.get
//      }
      //def knownValues = currentOutputs ++ knownInputs
      val lookup = NoLookup //lookupFunction(knownValues, wdlFunctions, outputResolver, shards, output)
      val coerced = output.requiredExpression.evaluate(lookup, wdlFunctions) flatMap output.wdlType.coerceRawValue
      val workflowOutput = output -> coerced

      outputMap + workflowOutput
    }) map { case (k, v) => k -> v }

    TryUtil.sequenceMap(evaluatedOutputs, "Failed to evaluate workflow outputs.\n")
  }
}

object Workflow {
  trait WorkflowMaker[A] {
    def makeWorkflow(input: A): Workflow
  }
}
