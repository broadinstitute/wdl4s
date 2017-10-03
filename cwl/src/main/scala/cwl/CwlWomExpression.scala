package cwl

import cats.syntax.validated._
import lenthall.validation.ErrorOr.ErrorOr
import lenthall.validation.Validation._
import wdl.types._
import wdl.values.{WdlFile, WdlString, WdlValue}
import wom.expression.{IoFunctionSet, WomExpression}

import scala.util.{Failure, Try}

sealed trait CwlWomExpression extends WomExpression {

  def cwlExpressionType: WdlType

  override def evaluateType(inputTypes: Map[String, WdlType]): ErrorOr[WdlType] = cwlExpressionType.validNel
}

case class CommandOutputExpression(outputBinding: CommandOutputBinding,
                                   override val cwlExpressionType: WdlType) extends CwlWomExpression {

  override def evaluateValue(inputValues: Map[String, WdlValue], ioFunctionSet: IoFunctionSet): ErrorOr[WdlValue] = {
    val parameterContext = ParameterContext.Empty.withInputs(inputValues, ioFunctionSet)

    val wdlValue = outputBinding.commandOutputBindingToWdlValue(parameterContext, ioFunctionSet)
    cwlExpressionType.coerceRawValue(wdlValue).toErrorOr
  }

  override def inputs: Set[String] = ???

  override def evaluateFiles(inputTypes: Map[String, WdlValue], ioFunctionSet: IoFunctionSet, coerceTo: WdlType): ErrorOr[Set[WdlFile]] ={
    val pc = ParameterContext.Empty.withInputs(inputTypes, ioFunctionSet)
    val wdlValue = outputBinding.commandOutputBindingToWdlValue(pc, ioFunctionSet)
    WdlFileType.coerceRawValue(wdlValue).flatMap{
      case WdlString(value) => println("got wdl string"); Try(Set(WdlFile(value, true)))
      case other: WdlValue => Failure(new RuntimeException(s":( we saw $other and coulnd't convert to a globfile"))
    }.toErrorOr
    //coerceTo.coerceRawValue(wdlValue).toErrorOr
    /*
    outputBinding match {
      case CommandOutputBinding(Some(glob), Some(true), Some(Inl(expression))) =>
        expression.fold(ExpressionEvaluator).apply(pc)

    }

    val glob: Option[Glob] = outputBinding.glob.flatMap(_.select[String])
    outputBinding.loadContents
    outputBinding.outputEval

    val function: (String, String) => Seq[String] = ioFunctionSet.glob _

    Valid(Set(WdlFile()))
    */
  }
}

object CwlWomExpression {

}
