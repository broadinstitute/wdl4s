package wdl4s.cwl

import eu.timepit.refined.api.Refined
import shapeless.{:+:, CNil}
import wdl4s.cwl.CwlType.CwlType
import eu.timepit.refined.string._
import eu.timepit.refined._
import io.circe.refined._

trait TypeAliases {
  type WorkflowStepInputId = String

  type WorkflowStepInputSource = String :+: Array[String] :+: CNil

  /**
    * These are supposed to be valid ECMAScript Expressions.
    * See http://www.commonwl.org/v1.0/Workflow.html#Expressions
    */
  type ECMAScriptExpression = String Refined MatchesRegex[W.`"$({.*}|{.*})"`.T]

  type Requirement =
    InlineJavascriptRequirement :+:
      SchemaDefRequirement :+:
      DockerRequirement :+:
      SoftwareRequirement :+:
      InitialWorkDirRequirement :+:
      EnvVarRequirement :+:
      ShellCommandRequirement :+:
      ResourceRequirement :+:
      SubworkflowFeatureRequirement :+:
      ScatterFeatureRequirement :+:
      MultipleInputFeatureRequirement :+:
      StepInputExpressionRequirement :+:
      CNil

  type MyriadInputType =
    CwlType :+:
      InputRecordSchema :+:
      InputEnumSchema :+:
      InputArraySchema :+:
      String :+:
      Array[
        CwlType :+:
          InputRecordSchema :+:
          InputEnumSchema :+:
          InputArraySchema :+:
          String :+:
          CNil
        ] :+:
      CNil

  type MyriadOutputType =
    CwlType :+:
      OutputRecordSchema :+:
      OutputEnumSchema :+:
      OutputArraySchema :+:
      String :+:
      Array[
        CwlType :+:
          OutputRecordSchema :+:
          OutputEnumSchema :+:
          OutputArraySchema :+:
          String :+:
          CNil
        ] :+:
      CNil

  type MyriadCommandInputType =
    CwlType :+:
      CommandInputRecordSchema :+:
      CommandInputEnumSchema :+:
      CommandInputArraySchema :+:
      String :+:
      Array[
        CwlType  :+:
          CommandInputRecordSchema :+:
          CommandInputEnumSchema :+:
          CommandInputArraySchema :+:
          String :+:
          CNil
        ] :+:
      CNil

  type WorkflowInput =
    Map[InputParameter#Id, InputParameter] :+:
      Map[InputParameter#Id, InputParameter#`type`] :+:
      Array[InputParameter] :+:
      CNil

  type WorkflowOutput =
    Map[WorkflowOutputParameter#Id, WorkflowOutputParameter] :+:
      Map[WorkflowOutputParameter#Id, WorkflowOutputParameter#`type`] :+:
      Array[WorkflowOutputParameter] :+:
      CNil

  type WorkflowSteps =
    Map[String, WorkflowStep] :+:
      Array[WorkflowStep] :+:
      CNil

}
