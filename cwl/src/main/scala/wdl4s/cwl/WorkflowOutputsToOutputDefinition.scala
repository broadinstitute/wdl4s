package wdl4s.cwl

import shapeless.Poly1
import wdl4s.wom.callable.Callable.OutputDefinition
import wdl4s.wom.expression.PlaceholderExpression

object WorkflowOutputsToOutputDefinition extends Poly1 {

  def mungeId(fullyQualifiedName: String): String =
    fullyQualifiedName.substring(fullyQualifiedName.lastIndexOf("/") + 1)

  def fullIdToOutputDefintition(fullyQualifiedName: String, typeMap: WdlTypeMap) = {

    //we want to only look at the id, not the filename
    val lookupId = WorkflowStepOutputId(fullyQualifiedName).outputId

    OutputDefinition(fullyQualifiedName, typeMap(lookupId), PlaceholderExpression(typeMap(lookupId)))
  }

  implicit def a = at[Array[WorkflowStepOutput]] { outputs =>
    (typeMap: WdlTypeMap) =>
      outputs.map(output => fullIdToOutputDefintition(output.id, typeMap)).toSet
  }

  implicit def b = at[Array[String]] { outputs =>
    (typeMap: WdlTypeMap) =>
      outputs.map(fullIdToOutputDefintition(_, typeMap)).toSet
  }

}

