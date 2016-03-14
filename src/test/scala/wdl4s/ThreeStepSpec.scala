package wdl4s

import org.scalatest.{FlatSpec, Matchers}
import wdl4s.expression.NoFunctions
import wdl4s.types.{WdlFileType, WdlIntegerType}
import wdl4s.values.{WdlFile, WdlString}

import scala.util.Failure

class ThreeStepSpec extends FlatSpec with Matchers {
  val namespace = WdlNamespaceWithWorkflow.load(SampleWdl.ThreeStep.wdlSource())

  "WdlNamespace" should "Be able to coerce inputs" in {
    namespace.coerceRawInputs(Map("three_step.cgrep.pattern" -> "abc")).get shouldEqual Map(
      "three_step.cgrep.pattern" -> WdlString("abc")
    )
  }
  it should "Report an error when not all inputs are specified" in {
    namespace.coerceRawInputs(Map.empty[FullyQualifiedName, Any]) shouldBe a[Failure[_]]
  }

  "Binding Workflow" should "Have correct name for workflow" in {
    namespace.workflow.unqualifiedName shouldEqual "three_step"
  }
  it should "Have correct FQN" in {
    namespace.workflow.fullyQualifiedName shouldEqual "three_step"
  }
  it should "Have no parent" in {
    namespace.workflow.parent shouldEqual Option(namespace)
  }
  it should "Have three 'Call' objects" in {
    namespace.workflow.calls.size shouldEqual 3
  }
  it should "Have three outputs" in {
    namespace.workflow.outputs.size shouldEqual 3
  }
  it should "Be able to find calls by name" in {
    namespace.workflow.findCallByName("ps").map(_.fullyQualifiedName) shouldEqual Option("three_step.ps")
    namespace.workflow.findCallByName("wc").map(_.fullyQualifiedName) shouldEqual Option("three_step.wc")
    namespace.workflow.findCallByName("cgrep").map(_.fullyQualifiedName) shouldEqual Option("three_step.cgrep")
  }

  "Binding Tasks" should "Have three task definitions" in {
    namespace.tasks.size shouldEqual 3
  }

  it should "Have a task with name 'wc'" in {
    val task = namespace.findTask("wc") getOrElse fail("No 'wc' task found")
    task.name shouldEqual "wc"
    task.declarations.map(_.toWdlString) shouldEqual Vector("File in_file")
    task.instantiateCommand(Map("wc.in_file" -> WdlFile("/path/to/file")), NoFunctions).get shouldEqual "cat /path/to/file | wc -l"
    task.outputs.size shouldEqual 1
    task.outputs.head.unqualifiedName shouldEqual "count"
    task.outputs.head.wdlType shouldEqual WdlIntegerType
  }
  it should "Have a task with name 'cgrep'" in {
    val task = namespace.findTask("cgrep") getOrElse fail("No 'cgrep' task found")
    task.name shouldEqual "cgrep"
    task.declarations.map(_.toWdlString) shouldEqual Vector(
      "String pattern",
      "File in_file"
    )
    task.instantiateCommand(Map("cgrep.pattern" -> WdlString("^...$"), "cgrep.in_file" -> WdlFile("/path/to/file")),
      NoFunctions).get shouldEqual "grep '^...$' /path/to/file | wc -l"
    task.outputs.size shouldEqual 1
    task.outputs.head.unqualifiedName shouldEqual "count"
    task.outputs.head.wdlType shouldEqual WdlIntegerType
  }
  it should "Have a task with name 'ps'" in {
    val task = namespace.findTask("ps") getOrElse fail("No 'ps' task found")
    task.name shouldEqual "ps"
    task.declarations shouldEqual Vector()
    task.instantiateCommand(Map(), NoFunctions).get shouldEqual "ps"
    task.outputs.size shouldEqual 1
    task.outputs.head.unqualifiedName shouldEqual "procs"
    task.outputs.head.wdlType shouldEqual WdlFileType
  }
}
