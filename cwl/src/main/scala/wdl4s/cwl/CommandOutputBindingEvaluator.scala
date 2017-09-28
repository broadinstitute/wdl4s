package wdl4s.cwl

import scala.Function._
import shapeless._
import wdl4s.cwl.ExpressionEvaluator.{ECMAScriptExpression, ECMAScriptFunction}
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
    implicit def caseECMAScript: Case.Aux[Expression, OutputEvalHandler] = {
      at[Expression] { ecmaScript =>
        (parameterContext: ParameterContext) =>
          ecmaScript.fold(ExpressionToWdlValue).apply(parameterContext)
      }
    }

    implicit def caseString: Case.Aux[String, OutputEvalHandler] = {
      at[String] { string => const(WdlString(string)) }
    }
  }
}

object ExpressionToWdlValue extends Poly1 {
  implicit def script = at[ECMAScriptExpression] { e =>
    (parameterContext: ParameterContext) =>
      ExpressionEvaluator.evalExpression(e.value, parameterContext)
  }

  implicit def function = at[ECMAScriptFunction] { f =>
    (parameterContext: ParameterContext) =>
      ExpressionEvaluator.evalExpression(f.value, parameterContext)
  }
}
