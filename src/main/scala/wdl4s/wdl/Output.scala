package wdl4s.wdl

import wdl4s.wom.callable.Callable

trait Output extends DeclarationInterface {
  def requiredExpression: WdlExpression

  override val expression = Option(requiredExpression)

  def toWom = Callable.OutputDefinition(unqualifiedName, wdlType, requiredExpression)
}
