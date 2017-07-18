package wdl4s.cwl

import shapeless._
import syntax.singleton._
import wdl4s.cwl.CommandLineTool.{Argument, BaseCommand, Inputs, StringOrExpression}
import wdl4s.cwl.CommandOutputBinding.Glob
import wdl4s.cwl.WorkflowStep.{Outputs, Run}
import io.circe.syntax._
import io.circe.yaml._
import io.circe.yaml.syntax._
import org.scalatest._

class ThreeStepSpec extends FlatSpec with Matchers {
  val namespace = "threestep"
  it should "print nicely " in {

    val inlineJScriptRequirements = Option(Array(
      Coproduct[Requirement](ShellCommandRequirement()),
      Coproduct[Requirement](InlineJavascriptRequirement())))

  /**
    * PS
    */
  val psOutputBinding = CommandOutputBinding(glob = Some(Coproduct[Glob](("ps-stdOut.txt"))))

  val psOutputParameter =
    CommandOutputParameter(
      id = "ps-stdOut",
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
    out = Coproduct(Array("ps-stdOut"))
  )


  /**
    * Cgrep
    */
  val patternInput = CommandInputParameter(id = Some("pattern"), `type` = Some(Coproduct(CwlType.String)))

  val fileInput = CommandInputParameter(id = Some("file"), `type` = Some(Coproduct(CwlType.File)))

  def clb: String => CommandLineTool.Argument=
    s => Coproduct[Argument](CommandLineBinding(valueFrom = Some(Coproduct[StringOrExpression](s)), shellQuote  = Some(false)))

  val cgrepArgs = Array(
    clb("grep"),
    clb("$(inputs.pattern)"),
    clb("$(inputs.file)"),
    clb("|"),
    clb("wc"),
    clb("-l")
  )


  val cgrepOutputBinding = CommandOutputBinding(glob = Some(Coproduct[Glob]("cgrep-stdOut.txt")))

  val cgrepOutputParameter = CommandOutputParameter(id = "cgrep-stdOut", `type` = Some(Coproduct(CwlType.File)), outputBinding = Some(cgrepOutputBinding))

  val cgrepClt = CommandLineTool(
    inputs = Coproduct[CommandLineTool.Inputs](Array(patternInput, fileInput)),
    outputs = Coproduct(Array(cgrepOutputParameter)),
    `class` = "CommandLineTool".narrow,
    arguments = Some(cgrepArgs),
    stdout = Some(Coproduct[StringOrExpression]("cgrep-stdOut.txt")),
    requirements = inlineJScriptRequirements
  )

  val patternCgrepWorkFlowStepInput = WorkflowStepInput(id = "pattern", source = Some(Coproduct("#pattern")))

  val fileCgrepWorkflowStepInput = WorkflowStepInput(id = "file", source = Some(Coproduct("ps/ps-stdOut")))

  val grepWfStep  = WorkflowStep(
    id = Some("cgrep"),
    in = Coproduct(Array(patternCgrepWorkFlowStepInput, fileCgrepWorkflowStepInput)),
    out = Coproduct[Outputs](Array(WorkflowStepOutput("cgrep-stdOut"))),
    run = Coproduct[Run](cgrepClt))


  /**
    * WC
    */
  val wcFileCommandInput = CommandInputParameter(
    id = Some("file"),
    `type` = Some(Coproduct(CwlType.File)))

  val wcArgs =
    Array(
      clb("cat"),
      clb("${inputs.file}"),
      clb("|"),
      clb("wc"),
      clb("-l")
      )

  val wcCltOutput = CommandOutputParameter(
    id = "wc-stdOut",
    `type` = Some(Coproduct(CwlType.File)),
    outputBinding = Some(CommandOutputBinding(glob = Some(Coproduct[Glob]("wc-stdOut.txt")))))

  val wcClt =
    CommandLineTool(
      `class` = "CommandLineTool".narrow,
      stdout = Some(Coproduct[StringOrExpression]("wc-stdOut.txt")),
      inputs = Coproduct(Array(wcFileCommandInput)),
      outputs = Coproduct(Array(wcCltOutput)),
      arguments = Some(wcArgs),
      requirements = inlineJScriptRequirements
    )


  val wcWorkflowInput = WorkflowStepInput(
    id = "file",
    source = Some(Coproduct("ps/ps-stdOut")))

  val wcWorkflowStep = WorkflowStep(
    id = Some("wc"),
    in = Coproduct[WorkflowStep.Inputs](Array(wcWorkflowInput)),
    out = Coproduct[WorkflowStep.Outputs](Array(WorkflowStepOutput("wc-stdOut"))),
    run = Coproduct[WorkflowStep.Run](wcClt))


  /**
    * Workflow
    */
  val outputCgrep =
    WorkflowOutputParameter(
      id = Some("cgrep-stdOut"),
      `type` = Some(Coproduct[MyriadOutputType](CwlType.File)),
      outputSource = Some(Coproduct("#cgrep/cgrep-stdOut"))
    )

  val outputWc =
    WorkflowOutputParameter(
      id = Some("wc-stdOut"),
      `type` = Some(Coproduct[MyriadOutputType](CwlType.File)),
      outputSource = Some(Coproduct("#wc/wc-stdOut"))
    )

  val _outputs = Coproduct[WorkflowOutput](Array(outputCgrep, outputWc))

  val workflowPatternInput = InputParameter(id = Some("pattern"), `type` = Some(Coproduct[MyriadInputType](CwlType.String)))

  val _inputs = Coproduct[WorkflowInput](Array(workflowPatternInput))

  val threeStepWorkflow =
    new Workflow(
      Some(CwlVersion.Version1),
      `class` = "Workflow".narrow,
      inputs = _inputs,
      outputs = _outputs,
      steps = Coproduct[WorkflowSteps](Array(psWfStep, grepWfStep, wcWorkflowStep)))

    val yaml = encodeWorkflow(threeStepWorkflow)

    println(yaml)


    true shouldBe true
  }

}
