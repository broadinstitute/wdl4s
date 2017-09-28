package wdl4s.wdl.wom

import cats.syntax.either._
import lenthall.validation.Checked._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
import shapeless.Coproduct
import wdl4s.wdl.types.{WdlIntegerType, WdlStringType}
import wdl4s.wdl.values.{WdlFile, WdlInteger, WdlOptionalValue, WdlString, WdlValue}
import wdl4s.wdl.{WdlNamespace, WdlNamespaceWithWorkflow}
import wdl4s.wom.executable.Executable.ResolvedExecutableInputs
import wdl4s.wom.graph.Graph.ResolvedExecutableInput

class WdlInputValidationSpec extends FlatSpec with Matchers with BeforeAndAfterAll with TableDrivenPropertyChecks {

  behavior of "WDL Wom executable"

  val wdlWorkflow =
    """
      |task t {
      |  String t1
      |  Int? t2
      |  command { ... }
      |}
      |
      |workflow w {
      |  File w1
      |  String? w2
      |  
      |  scatter(i in range(5)) {
      |    call t as u
      |  }
      |  
      |  call t
      |}
    """.stripMargin

  val namespace = WdlNamespace.loadUsingSource(wdlWorkflow, None, None).get.asInstanceOf[WdlNamespaceWithWorkflow]
  val executable = namespace.womExecutable.getOrElse(fail("Failed to build a wom executable"))
  val graph = executable.graph.getOrElse(fail("Failed to build a wom graph"))

  val w1OutputPort = graph.externalInputNodes.find(_.fullyQualifiedIdentifier == "w.w1").getOrElse(fail("Failed to find an input node for w1")).singleOutputPort
  val w2OutputPort = graph.externalInputNodes.find(_.fullyQualifiedIdentifier == "w.w2").getOrElse(fail("Failed to find an input node for w2")).singleOutputPort
  val t1OutputPort = graph.externalInputNodes.find(_.fullyQualifiedIdentifier == "w.t.t1").getOrElse(fail("Failed to find an input node for t1")).singleOutputPort
  val t2OutputPort = graph.externalInputNodes.find(_.fullyQualifiedIdentifier == "w.t.t2").getOrElse(fail("Failed to find an input node for t2")).singleOutputPort
  val u1OutputPort = graph.externalInputNodes.find(_.fullyQualifiedIdentifier == "w.u.t1").getOrElse(fail("Failed to find an input node for u1")).singleOutputPort
  val u2OutputPort = graph.externalInputNodes.find(_.fullyQualifiedIdentifier == "w.u.t2").getOrElse(fail("Failed to find an input node for u2")).singleOutputPort

  def validate(inputFile: String) = {
    executable.validateWorkflowInputs(inputFile)
  }

  it should "validate workflow inputs" in {
    val validations = Table(
      ("inputFile", "expectedResult"),
      (
        """
          |{
          |  "w.w1": "my_file.txt",
          |  "w.t.t1": "helloT",
          |  "w.u.t1": "helloU"
          |}
        """.stripMargin,
        Map (
          w1OutputPort -> Coproduct[ResolvedExecutableInput](WdlFile("my_file.txt"): WdlValue),
          w2OutputPort -> Coproduct[ResolvedExecutableInput](WdlOptionalValue.none(WdlStringType): WdlValue),
          t1OutputPort -> Coproduct[ResolvedExecutableInput](WdlString("helloT"): WdlValue),
          t2OutputPort -> Coproduct[ResolvedExecutableInput](WdlOptionalValue.none(WdlIntegerType): WdlValue),
          u1OutputPort -> Coproduct[ResolvedExecutableInput](WdlString("helloU"): WdlValue),
          u2OutputPort -> Coproduct[ResolvedExecutableInput](WdlOptionalValue.none(WdlIntegerType): WdlValue)
        ).validNelCheck
      ),
      (
        """
          |{
          |  "w.w1": "my_file.txt",
          |  "w.w2": "inputString",
          |  "w.t.t1": "helloT",
          |  "w.t.t2": 5,
          |  "w.u.t1": "helloU",
          |  "w.u.t2": 6
          |}
        """.stripMargin,
        Map (
          w1OutputPort -> Coproduct[ResolvedExecutableInput](WdlFile("my_file.txt"): WdlValue),
          w2OutputPort -> Coproduct[ResolvedExecutableInput](WdlOptionalValue(WdlString("inputString")): WdlValue),
          t1OutputPort -> Coproduct[ResolvedExecutableInput](WdlString("helloT"): WdlValue),
          t2OutputPort -> Coproduct[ResolvedExecutableInput](WdlOptionalValue(WdlInteger(5)): WdlValue),
          u1OutputPort -> Coproduct[ResolvedExecutableInput](WdlString("helloU"): WdlValue),
          u2OutputPort -> Coproduct[ResolvedExecutableInput](WdlOptionalValue(WdlInteger(6)): WdlValue)
        ).validNelCheck
      ),
      (
        """
          |{
          |}
        """.stripMargin,
        Set(
          "Required workflow input 'w.t.t1' not specified",
          "Required workflow input 'w.u.t1' not specified",
          "Required workflow input 'w.w1' not specified"
        ).asLeft[ResolvedExecutableInputs]
      )
    )

    forAll(validations) { (inputSource, expectation) =>
      // The order in the Nel is not important, so make it a Set to check that all the expected failure messages are here, regardless of their order
      validate(inputSource).leftMap(_.toList.toSet) shouldBe expectation
    }
  }

}
