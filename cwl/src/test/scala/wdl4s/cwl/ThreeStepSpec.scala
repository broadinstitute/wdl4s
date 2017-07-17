package wdl4s.cwl

import org.scalatest.FlatSpec
import shapeless.Coproduct
import shapeless.syntax.inject._
import shapeless.ops.coproduct.Inject
import mouse.all._
import wdl4s.cwl.CommandLineTool.BaseCommand
import wdl4s.cwl.WorkflowStep.WorkflowStepRun

class ThreeStepSpec extends FlatSpec {

  "Cwl Three step" should "convert to Wom" in {

    /**
     * PS
     */
    val psClt = CommandLineTool(
      `class` = "CommandLineTool",
      baseCommand = Some(Coproduct[BaseCommand]("ps"))
      stdOut = Some("ps-stdOut.txt"))

    val psOutputBinding = CommandOutputBinding(glob = Some(Coproduct("ps-stdOut.txt"))

    val psOutputParameter = CommandOutputParameter(id = "stdOut", `type` = CwlType.File, outputBinding = Some(psCLTOutputBinding))

    val psWfStep  = WorkflowStep(
      id = Some("ps"),
      inputs = Coproduct[CommandLineTool.Inputs](Array.empty),
      outputs = Coproduct[CommandLineTool.Outputs](Array(psOutputParameter)),
      run = Coproduct[WorkflowStepRun](psClt))


    /**
     * Cgrep step
     */
    val patternInput = CommandInputParameter(id = Some("pattern"), `type` = Some(CwlType.String))

    val fileInput = CommandInputParameter(id = Some("file"), `type` = Some(CwlType.File))

    val grepOutputBinding = CommandOutputBinding(
      glob = Some(Coproduct("grepout.txt")),
      loadContents = Some(true),
      outputEval = Some(Coproduct("$(parseInt(self[0].contents))"))

    val grepCommandLineBinding = CommandLineBinding(valueFrom = Some("grep"), shellQuote  = Some(false))
    val patternCommandLineBinding = CommandLineBinding(valueFrom = Some("$(inputs.pattern)"), shellQuote  = Some(false))
    val inFileCommandLineBinding = CommandLineBinding(valueFrom = Some("$(inputs.file)"), shellQuote  = Some(false))
    val wcL = CommandLineBinding(valueFrom = Some("| wc -l"), shellQuote  = Some(false))


    val countGrepOutput = WorkflowStepOutput(id = Some("count"))

    val cgrepOutputBinding = CommandOutputBinding(glob = Some(Coproduct("cgrep-stdOut.txt"))

    val cgreppsOutputParameter = CommandOutputParameter(id = "stdOut", `type` = CwlType.File, outputBinding = Some(psCLTOutputBinding))

    val cgrepClt = CommandLineTool(
      `class` = "CommandLineTool",
      inputs = Coproduct[CommandLineTool.Inputs](Array(patternInput, fileInput)),
      arguments = Some(Coproduct(Array(grepCommandLineBinding, patternCommandLineBinding, inFileCommandLineBinding, wcL))),
      stdOut = Some("cgrep-stdOut.txt"))

    val patternCgrepWorkFlowStepInput = WorkflowStepInput(id = "pattern")

    val fileCgrepWorkflowStepInput = WorkflowStepInput(id = "file", source = Some("ps/stdOut"))

    val grepWfStep  = WorkflowStep(
      id = Some("cgrep"),
      inputs = Coproduct[CommandLineTool.Inputs](Array(patternCgrepWorkFlowStepInput, fileCgrepWorkflowStepInput)),
      outputs = Coproduct[CommandLineTool.Outputs](Array(countGrepOutput)),
      run = Coproduct[WorkflowStepRun](cgrepClt))


    /**
     * WC
     */
    val wcFileWorkflowStepInput = CommandInputParameter(id = Some("in_file"), `type` = Some(Cwl.File))

    val cwCatCltBinding = CommandLineBinding(valueFrom = Some(Coproduct("cat")), shellQuote = Some(false))

    val cwCatFileCltBinding = CommandLineBinding(valueFrom = Some(Coproduct("${inputs.file")), shellQuote = Some(false))

    val cwPipeCltBinding = CommandLineBinding(valueFrom = Some(Coproduct("| wc -l")), shellQuote = Some(false))

    val wcOutputBinding = CommandOutputBinding(glob = some("stdOut.txt"))

    val wcClt = CommandLineTool(
      `class` = "CommandLineTool",
      stdOut = Some("wc-stdOut.txt")
      inputs = Coproduct(Array(wcFileWorkflowStepInput)),
      arguments = Some(Coproduct(Array(cwCatFileCltBinding, cwCatFileCltBinding, cwPipeCltBinding))


    val wcWorkflowInput = WorkflowStepInput(id = "in_file", source = Some("ps/stdOut"))

    val wcWorkflowStep = WorkflowStep(
      id = Some("wc"),
      inputs = Coproduct(Array(wcWorkflowInput))
      outputs = Coproduct[CommandLineTool.Outputs](Array(wcWorkflowStepOutput)),
      run = Coproduct(wcClt)


    /**
     * Workflow
     */
    val outputCgrep = WorkflowOutputParameter(id = Some("cgrep.count"), `type` = Some(Coproduct[MyriadOutputType](CwlType.Int)))

    val outputWc = WorkflowOutputParameter(id = Some("wc.count"), `type` = Some(Coproduct[MyriadOutputType](CwlType.Int)))

    val _outputs = Coproduct[WorkflowInput](Array(outputCgrep, outputWc))

    val workflowPatternInput: InputParameter = ???

    val _inputs = Coproduct[WorkflowInput](Array(workflowPatternInput))

    val m = new Workflow(
      None,
      `class` = "Workflow",
      inputs = _inputs,
      outputs = _outputs,
      steps = Coproduct[WorkflowSteps](Array(psWfStep))
  }
}
