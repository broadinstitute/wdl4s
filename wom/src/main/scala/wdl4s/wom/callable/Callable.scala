package wdl4s.wom.callable

import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.wdl.types.{WdlOptionalType, WdlType}
import wdl4s.wom.callable.Callable._
import wdl4s.wom.expression.WomExpression
import wdl4s.wom.graph.Graph


trait Callable {
  def name: String

  def graph: ErrorOr[Graph]
  def inputs: List[_ <: InputDefinition]
}

object Callable {
  sealed trait InputDefinition {
    def name: String
    def womType: WdlType
  }

  /**
    * InputDefinitions that translate to GraphInputNodes when left unconnected
    */
  sealed trait GraphInputDefinition extends InputDefinition

  final case class OptionalInputDefinition(name: String, womType: WdlOptionalType) extends GraphInputDefinition
  final case class OptionalInputDefinitionWithDefault(name: String, womType: WdlType, default: WomExpression) extends GraphInputDefinition
  final case class RequiredInputDefinition(name: String, womType: WdlType) extends GraphInputDefinition
    
  // note that DeclaredInputDefinition is NOT a GraphInputDefinition which means it can't be satisfied using a value external to the graph
  // (specifically there won't be any GraphInputNode created for it if it can't be satisfied otherwise)
  final case class DeclaredInputDefinition(name: String, womType: WdlType, expression: WomExpression) extends InputDefinition
  
  final case class OutputDefinition(name: String, womType: WdlType, expression: WomExpression)
}
