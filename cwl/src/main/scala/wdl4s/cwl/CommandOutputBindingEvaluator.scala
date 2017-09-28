package wdl4s.cwl

import scala.Function._
import shapeless._
import wdl4s.wdl.values._
import wdl4s.wom.expression.IoFunctionSet

/*
CommandOutputBinding.glob:
Find files relative to the output directory, using POSIX glob(3) pathname matching. If an array is provided, find
files that match any pattern in the array. If an expression is provided, the expression must return a string or an
array of strings, which will then be evaluated as one or more glob patterns. Must only match and return files which
actually exist.

http://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputBinding
 */
object CommandOutputBindingEvaluator {

  private type OutputEvalHandler = ParameterContext => WdlValue

  def commandOutputBindingToWdlValue(commandOutputBinding: CommandOutputBinding,
                                     parameterContext: ParameterContext,
                                     ioFunctionSet: IoFunctionSet): WdlValue = {
    /*
    TODO: WOM: If the file cwl.outputs.json is present in the output, outputBinding is ignored.
    - http://www.commonwl.org/v1.0/CommandLineTool.html#File
     */

    val paths: Seq[String] = commandOutputBinding.glob map { globValue =>
      GlobEvaluator.globPaths(globValue, parameterContext, ioFunctionSet)
    } getOrElse {
      Vector.empty
    }

    val loadContents: Boolean = commandOutputBinding.loadContents getOrElse false

    val arrayOfCwlFileMaps = CwlFileEvaluator.pathsToArrayOfCwlFileMaps(paths, ioFunctionSet, loadContents)
    val outputEvalParameterContext = parameterContext.copy(self = arrayOfCwlFileMaps)

    commandOutputBinding.outputEval match {
      case Some(outputEvalCoproduct) =>
        outputEvalCoproduct.fold(OutputEvalToWdlValue).apply(outputEvalParameterContext)
      case None =>
        // Return the WdlArray of file paths, three_step.ps needs this for stdout output.
        // There will be conversion required between this Array[File] output type and the requested File.
        arrayOfCwlFileMaps
    }
  }

  object OutputEvalToWdlValue extends Poly1 {
    implicit def caseECMAScript: Case.Aux[ECMAScript, OutputEvalHandler] = {
      at[ECMAScript] { ecmaScript =>
        (parameterContext: ParameterContext) =>
          ExpressionEvaluator.evalExpression(ecmaScript.value, parameterContext)
      }
    }

    implicit def caseECMAFunction: Case.Aux[ECMAFunction, OutputEvalHandler] = {
      at[ECMAFunction] { ecmaFunction =>
        (parameterContext: ParameterContext) =>
          ExpressionEvaluator.evalExpression(ecmaFunction.value, parameterContext)
      }
    }

    implicit def caseString: Case.Aux[String, OutputEvalHandler] = {
      at[String] { string => const(WdlString(string)) }
    }
  }
}
