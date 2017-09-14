package wdl4s.wom.callable

import wdl4s.wdl.types.{WdlIntegerType, WdlStringType}

object TaskDefinitionSpec {

  val noInputsOrOutputsTask = TaskDefinition(
    name = "foo",
    commandTemplate = Seq.empty,
    runtimeAttributes = null,
    meta = Map.empty,
    parameterMeta = Map.empty,
    outputs = List.empty,
    inputs = List.empty)

  val oneInputTask = TaskDefinition(
    name = "foo",
    commandTemplate = Seq.empty,
    runtimeAttributes = null,
    meta = Map.empty,
    parameterMeta = Map.empty,
    outputs = List.empty,
    inputs = List(Callable.RequiredInputDefinition("bar", WdlIntegerType)))

  val oneOutputTask = TaskDefinition(
    name = "foo",
    commandTemplate = Seq.empty,
    runtimeAttributes = null,
    meta = Map.empty,
    parameterMeta = Map.empty,
    outputs = List(Callable.OutputDefinition("bar", WdlStringType, null)),
    inputs = List.empty)
}
