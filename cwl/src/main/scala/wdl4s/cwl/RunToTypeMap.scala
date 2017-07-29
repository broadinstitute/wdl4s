package wdl4s.cwl

import shapeless.Poly1
import wdl4s.cwl.CwlType.CwlType
import wdl4s.wdl.types.WdlType

object RunToTypeMap extends Poly1 {
  def mungeId(fullyQualifiedId: String): String =
    fullyQualifiedId.substring(fullyQualifiedId.lastIndexOf("#") + 1,fullyQualifiedId.length())

  def handleCommandLine(clt: CommandLineTool):Map[String, WdlType] = {
    clt.outputs.toList.foldLeft(Map.empty[String,WdlType]) {
      (acc, out) =>
        acc ++
          out.
            `type`.
            flatMap(_.select[CwlType]).
            map(cwlTypeToWdlType).
            map(out.id -> _).
            toList.
            toMap
    }
  }
  implicit def commandLineTool =
    at[CommandLineTool] {
      clt =>
        (_: Map[String, Cwl]) =>
          handleCommandLine(clt)
    }

  implicit def string = at[String] {
    fileName =>
      (cwlMap: Map[String, Cwl]) =>
        cwlMap(fileName) match {
          case clt: CommandLineTool => handleCommandLine(clt)
          case wf: Workflow => handleWorkflow(wf)
        }
  }

  implicit def expressionTool = at[ExpressionTool] {
    et =>
      (cwlMap: Map[String, Cwl]) =>
         Map.empty[String, WdlType]
  }

  //TODO: run _.stepOutputs recursively
  def handleWorkflow(workflow: Workflow) = {
      Map.empty[String, WdlType]
  }

  implicit def workflow = at[Workflow] {
    wf =>
      (cwlMap: Map[String, Cwl]) =>
         handleWorkflow(wf)
  }
}

