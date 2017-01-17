package wdl4s.exception

import lenthall.exception.ThrowableAggregation
import wdl4s.{Declaration, GraphNode}
import wdl4s.types.WdlType

sealed trait LookupException { this: Exception => }

/**
  * When a variable does not reference any known scope in the namespace.
  */
final case class VariableNotFoundException(variable: String) extends Exception(s"Variable $variable not found") with LookupException

object VariableNotFoundException {
  def apply(declaration: Declaration): VariableNotFoundException = VariableNotFoundException.apply(declaration.fullyQualifiedName, declaration.wdlType)
  def apply(variable: String, variableType: WdlType): VariableNotFoundException= VariableNotFoundException(variable + variableType.toWdlString)
}

/**
  * When an unexpected exception occurred while attempting to resolve a variable.
  * Might act a single exception or aggregate multiple exceptions.
  */
class VariableLookupException(override val exceptionContext: String, override val throwables: List[Throwable] = List.empty) extends RuntimeException with ThrowableAggregation with LookupException

/**
  * Raised when attempting to resolve a variable in a scatter but the index could not be found.
  */
final case class ScatterIndexNotFound(message: String) extends VariableLookupException(message)

/**
  * Raised when attempting to resolve an output variable but the output resolver failed to return a value.
  */
final case class OutputVariableLookupException(node: GraphNode, index: Option[Int]) extends VariableLookupException(s"Could not find outputs for call ${node.fullyQualifiedName} at index $index")