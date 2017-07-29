package wdl4s.cwl

import shapeless.Poly1
import wdl4s.wom.callable.Callable.OutputDefinition
import wdl4s.wom.expression.PlaceholderExpression

object WorkflowOutputsToOutputDefinition extends Poly1 {

  def mungeId(fullyQualifiedName: String): String =
     fullyQualifiedName.substring(fullyQualifiedName.lastIndexOf("/") + 1,fullyQualifiedName.length())

  def fullIdToOutputDefintition(fullyQualifiedName: String, typeMap: TypeMap) = {

    //we want to only look at the id, not the filename
    val _id = fullyQualifiedName.substring(fullyQualifiedName.lastIndexOf("/") + 1,fullyQualifiedName.length())

    OutputDefinition(_id, typeMap(_id), PlaceholderExpression(typeMap(_id)))
  }

  implicit def a = at[Array[WorkflowStepOutput]] { o =>
    (typeMap: TypeMap) =>
      o.map(output => fullIdToOutputDefintition(output.id, typeMap)).toSet
  }

  implicit def b = at[Array[String]] { o =>
    (typeMap: TypeMap) =>
      o.map(fullIdToOutputDefintition(_, typeMap)).toSet
  }

}

