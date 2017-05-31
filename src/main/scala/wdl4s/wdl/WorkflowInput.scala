package wdl4s.wdl

import wdl4s.wdl.types.{WdlOptionalType, WdlType}

case class WorkflowInput(fqn: FullyQualifiedName, wdlType: WdlType) {
  val optional = wdlType.isInstanceOf[WdlOptionalType]
}
