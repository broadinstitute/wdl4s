package wdl4s.cwl

import org.scalatest.FlatSpec
import shapeless._
import syntax.singleton._
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand, Inputs, StringOrExpression}
import wdl4s.cwl.CommandOutputBinding.Glob
import wdl4s.cwl.WorkflowStep.{Outputs, Run}

class ThreeStepSpec extends FlatSpec {

  "Cwl Three step" should "convert to Wom" in {

    /**
     * PS
     */
    val psOutputBinding = CommandOutputBinding(glob = Some(Coproduct[Glob](("ps-stdOut.txt"))))

    val psOutputParameter =
      CommandOutputParameter(
        id = "stdOut",
        `type` = Option(Coproduct(CwlType.File)),
        outputBinding = Some(psOutputBinding))

    val psClt = CommandLineTool(
      `class` = "CommandLineTool".narrow,
      outputs = Coproduct[CommandLineTool.Outputs](Array(psOutputParameter)),
      baseCommand = Some(Coproduct[BaseCommand]("ps")),
      stdout = Some(Coproduct[StringOrExpression]("ps-stdOut.txt")),
    )


    val psWfStep  = WorkflowStep(
      id = Some("ps"),
      run = Coproduct[Run](psClt),
      out = Coproduct(Array("stdOut"))
    )


    /**
     * Cgrep
     */
    val patternInput = CommandInputParameter(id = Some("pattern"), `type` = Some(Coproduct(CwlType.String)))

    val fileInput = CommandInputParameter(id = Some("file"), `type` = Some(Coproduct(CwlType.File)))

    def clb: String => CommandLineTool.Argument=
      s => Coproduct[Argument](CommandLineBinding(valueFrom = Some(Coproduct[StringOrExpression](s)), shellQuote  = Some(false)))

    val cgrepArgs = Array(
      clb("grep '"),
      clb("$(inputs.pattern)"),
      clb("' "),
      clb("$(inputs.file)"),
      clb("| wc -l")
    )


    val cgrepOutputBinding = CommandOutputBinding(glob = Some(Coproduct[Glob]("cgrep-stdOut.txt")))

    val cgrepOutputParameter = CommandOutputParameter(id = "stdOut", `type` = Some(Coproduct(CwlType.File)), outputBinding = Some(cgrepOutputBinding))

    val cgrepClt = CommandLineTool(
      inputs = Coproduct[CommandLineTool.Inputs](Array(patternInput, fileInput)),
      outputs = Coproduct(Array(cgrepOutputParameter)),
      `class` = "CommandLineTool".narrow,
      arguments = Some(cgrepArgs),
      stdout = Some(Coproduct[StringOrExpression]("cgrep-stdOut.txt")))

    val patternCgrepWorkFlowStepInput = WorkflowStepInput(id = "pattern")

    val fileCgrepWorkflowStepInput = WorkflowStepInput(id = "file", source = Some(Coproduct("ps/stdOut")))

    val grepWfStep  = WorkflowStep(
      id = Some("cgrep"),
      in = Coproduct(Array(patternCgrepWorkFlowStepInput, fileCgrepWorkflowStepInput)),
      out = Coproduct[Outputs](Array(WorkflowStepOutput("count"))),
      run = Coproduct[Run](cgrepClt))


    /**
     * WC
     */
    val wcFileWorkflowStepInput = CommandInputParameter(id = Some("in_file"), `type` = Some(Coproduct(CwlType.File)))

    val wcArgs =
      Array(
        clb("cat"),
        clb("${inputs.file"),
        clb("| wc -l"),
        clb("stdOut.txt"))

    val wcCltOutput = CommandOutputParameter(
      id = "stdOut",
      `type` = Some(Coproduct(CwlType.File)),
      outputBinding = Some(CommandOutputBinding(glob = Some(Coproduct[Glob]("wc-stdOut.txt")))))

    val wcClt =
      CommandLineTool(
        `class` = "CommandLineTool".narrow,
        stdout = Some(Coproduct[StringOrExpression]("wc-stdOut.txt")),
        inputs = Coproduct(Array(wcFileWorkflowStepInput)),
        outputs = Coproduct(Array(wcCltOutput)),
        arguments = Some(wcArgs))


    val wcWorkflowInput = WorkflowStepInput(id = "in_file", source = Some(Coproduct("ps/stdOut")))

    val wcWorkflowStep = WorkflowStep(
      id = Some("wc"),
      in = Coproduct[WorkflowStep.Inputs](Array(wcWorkflowInput)),
      out = Coproduct[WorkflowStep.Outputs](Array(WorkflowStepOutput("count"))),
      run = Coproduct[WorkflowStep.Run](wcClt))


    /**
     * Workflow
     */
    val outputCgrep = WorkflowOutputParameter(id = Some("cgrep.count"), `type` = Some(Coproduct[MyriadOutputType](CwlType.Int)))

    val outputWc = WorkflowOutputParameter(id = Some("wc.count"), `type` = Some(Coproduct[MyriadOutputType](CwlType.Int)))

    val _outputs = Coproduct[WorkflowOutput](Array(outputCgrep, outputWc))

    val workflowPatternInput = InputParameter(id = Some("pattern"), `type` = Some(Coproduct[MyriadInputType](CwlType.String)))

    val _inputs = Coproduct[WorkflowInput](Array(workflowPatternInput))

    val threeStepWorkflow =
      new Workflow(
        None,
        `class` = "Workflow".narrow,
        inputs = _inputs,
        outputs = _outputs,
        steps = Coproduct[WorkflowSteps](Array(psWfStep)))

  }
}
