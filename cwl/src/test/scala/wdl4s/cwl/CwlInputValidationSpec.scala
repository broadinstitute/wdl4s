package wdl4s.cwl

import better.files.{File => BFile}
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
import shapeless.Coproduct
import wdl4s.cwl.CwlDecoder.decodeAllCwl
import wdl4s.wdl.values.{WdlBoolean, WdlFile, WdlFloat, WdlInteger, WdlString, WdlValue}
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.Graph.ResolvedExecutableInput
import wdl4s.wom.graph.GraphNodePort

class CwlInputValidationSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterAll {
  behavior of "CWL Wom executable"

  var cwlFile: BFile = _
  
  override def beforeAll(): Unit = {
    cwlFile = BFile.newTemporaryFile().write(
      """
        |cwlVersion: v1.0
        |class: Workflow
        |inputs:
        | w0:
        |   type: string
        |   default: "hi w0 !"
        | w1: File
        | w2:
        |   type: string
        |   default: "hi w2 !"
        | w3: int
        | w4: long
        | w5: double
        | w6: float
        | w7: boolean
        |steps: []    
        |outputs: []
      """.stripMargin
    )
  }
  
  override def afterAll(): Unit = {
    cwlFile.delete()
    ()
  }

  lazy val cwlWorkflow = decodeAllCwl(cwlFile).map {
    _.select[Workflow].get
  }.value.unsafeRunSync.fold(error => throw new RuntimeException(s"broken parse! msg was $error"), identity)

  lazy val womExecutable = cwlWorkflow.womExecutable.getOrElse(fail("Failed to build a wom executable"))
  lazy val graph = womExecutable.graph.getOrElse(fail("Failed to build wom graph"))

  lazy val w0OutputPort = graph.inputNodes.find(_.name == "w0").getOrElse(fail("Failed to find an input node for w0")).singleOutputPort
  lazy val w1OutputPort = graph.inputNodes.find(_.name == "w1").getOrElse(fail("Failed to find an input node for w1")).singleOutputPort
  lazy val w2OutputPort = graph.inputNodes.find(_.name == "w2").getOrElse(fail("Failed to find an input node for w2")).singleOutputPort
  lazy val w3OutputPort = graph.inputNodes.find(_.name == "w3").getOrElse(fail("Failed to find an input node for w3")).singleOutputPort
  lazy val w4OutputPort = graph.inputNodes.find(_.name == "w4").getOrElse(fail("Failed to find an input node for w4")).singleOutputPort
  lazy val w5OutputPort = graph.inputNodes.find(_.name == "w5").getOrElse(fail("Failed to find an input node for w5")).singleOutputPort
  lazy val w6OutputPort = graph.inputNodes.find(_.name == "w6").getOrElse(fail("Failed to find an input node for w6")).singleOutputPort
  lazy val w7OutputPort = graph.inputNodes.find(_.name == "w7").getOrElse(fail("Failed to find an input node for w7")).singleOutputPort
  
  def validate(inputFile: String): Map[GraphNodePort.OutputPort, ResolvedExecutableInput] = {
    womExecutable.validateWorkflowInputs(inputFile) match {
      case Left(errors) => fail(s"Failed to validate inputs: ${errors.toList.mkString(", ")}")
      case Right(resolvedInputs) => resolvedInputs
    }
  }

  it should "parse and validate a valid input file" in {
    val inputFile =
      """
        w1:
          class: File
          path: my_file.txt
        w2: hello !
        w3: 3
        w4: 4
        w5: 5.1
        w6: 6.1
        w7: true
      """.stripMargin

    val validInputs = validate(inputFile)

    // w0 has no input value in the input file, so it should fallback to the default value
    // TODO WOM: when we have string value for wom expression, check that it's "hi !"
    validInputs(w0OutputPort).select[WomExpression].isDefined shouldBe true
    validInputs(w1OutputPort) shouldBe Coproduct[ResolvedExecutableInput](WdlFile("my_file.txt"): WdlValue)
    validInputs(w2OutputPort) shouldBe Coproduct[ResolvedExecutableInput](WdlString("hello !"): WdlValue)
    validInputs(w3OutputPort) shouldBe Coproduct[ResolvedExecutableInput](WdlInteger(3): WdlValue)
    validInputs(w4OutputPort) shouldBe Coproduct[ResolvedExecutableInput](WdlInteger(4): WdlValue)
    validInputs(w5OutputPort) shouldBe Coproduct[ResolvedExecutableInput](WdlFloat(5.1F): WdlValue)
    validInputs(w6OutputPort) shouldBe Coproduct[ResolvedExecutableInput](WdlFloat(6.1F): WdlValue)
    validInputs(w7OutputPort) shouldBe Coproduct[ResolvedExecutableInput](WdlBoolean(true): WdlValue)
  }

  it should "not validate when required inputs are missing" in {
    val inputFile =
      """
        w2: hello !
      """.stripMargin

    womExecutable.validateWorkflowInputs(inputFile) match {
      case Right(booh) => fail(s"Expected failed validation but got valid input map: $booh")
      case Left(errors) => errors.toList.toSet shouldBe Set(
        "Required workflow input 'w1' not specified",
        "Required workflow input 'w3' not specified",
        "Required workflow input 'w4' not specified",
        "Required workflow input 'w5' not specified",
        "Required workflow input 'w6' not specified",
        "Required workflow input 'w7' not specified"
      )
    }
  }
}
