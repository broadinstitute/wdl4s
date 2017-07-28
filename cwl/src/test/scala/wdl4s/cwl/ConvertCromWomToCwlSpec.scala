package wdl4s.cwl

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.RuntimeAttributes
import wdl4s.wdl.command.CommandPart
import wdl4s.wom.callable.{Callable, TaskDefinition}
import wdl4s.wom.expression.Expression

class ConvertCromWomToCwlSpec extends FlatSpec with Matchers {

  it should "convert CromWOM TaskDefinition to CWL CommandLineTool" in {
    val taskDef = TaskDefinition(
      name = ??? : String,
      commandTemplate = ??? : Seq[CommandPart],
      runtimeAttributes = ??? : RuntimeAttributes,
      meta = ??? : Map[String, String],
      parameterMeta = ??? : Map[String, String],
      outputs = ??? : Set[Callable.OutputDefinition],
      inputs = ??? : Set[_ <: Callable.InputDefinition],
      declarations = ??? : List[(String, Expression)]
    )
    taskDef.toString.length > 3 shouldBe true
  }

}
