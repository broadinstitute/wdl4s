package wdl4s.cwl

import cats.data.Validated.Valid
import org.scalatest.{FlatSpec, Matchers}

class WomToCwlSpec extends FlatSpec with Matchers {

  private def compareCwlCmds(cmd1: CommandLineTool, cmd2: CommandLineTool): Unit = {
    cmd1.baseCommand shouldBe cmd2.baseCommand
    cmd1.arguments.map(_.toSeq) shouldBe cmd2.arguments.map(_.toSeq)
    cmd1.inputs.toSeq shouldBe cmd2.inputs.toSeq
    ()
  }

  it should "convert WOM examples to CWL" in {
    for(example <- WomToCwlExamples.examples) {
      val womTask = example.womTask
      val errorOrCwlCmdConverted = WomToCwl.toCwl(womTask)
      errorOrCwlCmdConverted.isValid shouldBe true
      val cwlCmdConverted = errorOrCwlCmdConverted.asInstanceOf[Valid[CommandLineTool]].a
      val cwlCmdExpected = example.cwlCmdExpected
      compareCwlCmds(cwlCmdConverted, cwlCmdExpected)
    }
  }

}
