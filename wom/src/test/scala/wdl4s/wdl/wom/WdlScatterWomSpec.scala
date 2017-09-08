package wdl4s.wdl.wom

import lenthall.collections.EnhancedCollections._
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import wdl4s.wdl.types.{WdlArrayType, WdlIntegerType, WdlStringType}
import wdl4s.wdl.{WdlNamespace, WdlNamespaceWithWorkflow}
import wdl4s.wom.graph.GraphNodePort.ScatterGathererPort
import wdl4s.wom.graph.{GraphInputNode, ScatterNode, _}

class WdlScatterWomSpec extends FlatSpec with Matchers {

  behavior of "WdlNamespaces with scatters"

  it should "convert gathered scatter outputs to workflow outputs" in {
    val scatterTest =
      """task foo {
        |  Int i
        |  command { ... }
        |  output {
        |    String out = i
        |  }
        |}
        |
        |workflow scatter_test {
        |  Array[Int] xs
        |  scatter (x in xs) {
        |    call foo { input: i = x }
        |  }
        |}""".stripMargin

    val namespace = WdlNamespace.loadUsingSource(scatterTest, None, None).get.asInstanceOf[WdlNamespaceWithWorkflow]
    import lenthall.validation.ErrorOr.ShortCircuitingFlatMap
    val scatterTestGraph = namespace.womExecutable.flatMap(_.graph)

    scatterTestGraph match {
      case Valid(g) => validateGraph(g)
      case Invalid(errors) => fail(s"Unable to build wom version of scatter foo from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }

    def validateGraph(workflowGraph: Graph) = {

      case class OuterGraphValidations(scatterNode: ScatterNode, xs_inputNode: GraphInputNode)
      def validateOuterGraph: OuterGraphValidations = {
        val scatterNode = workflowGraph.nodes.firstByType[ScatterNode].getOrElse(fail("Resulting graph did not contain a ScatterNode"))

        val xs_inputNode = workflowGraph.nodes.collectFirst {
          case gin: GraphInputNode if gin.name == "xs" => gin
        }.getOrElse(fail("Resulting graph did not contain the 'xs' GraphInputNode"))

        val foo_out_output = workflowGraph.nodes.collectFirst {
          case gon: GraphOutputNode if gon.name == "foo.out" => gon
        }.getOrElse(fail("Resulting graph did not contain the 'foo.out' GraphOutputNode"))
        foo_out_output.womType should be(WdlArrayType(WdlStringType))

        workflowGraph.nodes should be(Set(scatterNode, xs_inputNode, foo_out_output))
        OuterGraphValidations(scatterNode, xs_inputNode)
      }

      case class InnerGraphValidations(x_scatterCollectionInput: GraphInputNode, foo_out_innerOutput: GraphOutputNode)
      def validateInnerGraph(validatedOuterGraph: OuterGraphValidations): InnerGraphValidations = {
        val x_scatterCollectionInput = validatedOuterGraph.scatterNode.innerGraph.nodes.collectFirst {
          case gin: GraphInputNode if gin.name == "x" => gin
        }.getOrElse(fail("Scatter inner graph did not contain a GraphInputNode 'x'"))

        val foo_callNode = validatedOuterGraph.scatterNode.innerGraph.nodes.collectFirst {
          case c: TaskCallNode if c.name == "foo" => c
        }.getOrElse(fail("Scatter inner graph did not contain a call to 'foo'"))

        val foo_out_innerOutput = validatedOuterGraph.scatterNode.innerGraph.nodes.collectFirst {
          case gon: GraphOutputNode if gon.name == "foo.out" => gon
        }.getOrElse(fail("Scatter inner graph did not contain a GraphOutputNode 'foo.out'"))

        validatedOuterGraph.scatterNode.innerGraph.nodes should be(Set(x_scatterCollectionInput, foo_callNode, foo_out_innerOutput))
        InnerGraphValidations(x_scatterCollectionInput, foo_out_innerOutput)
      }

      def validateConnections(validatedOuterGraph: OuterGraphValidations, validatedInnerGraph: InnerGraphValidations) = {
        // The scatter collection links to its predecessor
        validatedOuterGraph.scatterNode.scatterVariableMapping.scatterInstantiatedExpression.inputPorts.map(_.upstream.graphNode) should be(Set(validatedOuterGraph.xs_inputNode))

        // The ScatterNode's "scatter variable mapping" links to the innerGraph's scatter variable input Node:
        validatedOuterGraph.scatterNode.scatterVariableMapping.graphInputNode eq validatedInnerGraph.x_scatterCollectionInput should be(true)

        // The ScatterNode's output port links to the inner graph's GraphOutputNode:
        validatedOuterGraph.scatterNode.outputMapping.toList match {
          case ScatterGathererPort(name, womType, outputToGather, _) :: Nil =>
            name should be("foo.out")
            womType should be(WdlArrayType(WdlStringType))
            outputToGather eq validatedInnerGraph.foo_out_innerOutput should be(true)
          case other => fail("Expected exactly one output to be gathered in this scatter but got:" + other.mkString("\n", "\n", "\n"))
        }
      }

      val outer = validateOuterGraph
      val inner = validateInnerGraph(outer)
      validateConnections(outer, inner)
    }
  }

  it should "convert inputs into inner graph inputs" in {
    val scatterTest =
      """task foo {
        |  Int i
        |  command { ... }
        |  output {
        |    String str_out = i
        |    Int int_out = i + 1
        |  }
        |}
        |
        |workflow scatter_test {
        |  Int x = 5
        |  Int y = 6
        |  Int z = 7
        |  scatter (s in [x, y]) {
        |    call foo { input: i = z }
        |  }
        |}""".stripMargin

    val namespace = WdlNamespace.loadUsingSource(scatterTest, None, None).get.asInstanceOf[WdlNamespaceWithWorkflow]
    import lenthall.validation.ErrorOr.ShortCircuitingFlatMap
    val scatterTestGraph = namespace.womExecutable.flatMap(_.graph)

    scatterTestGraph match {
      case Valid(g) => validateGraph(g)
      case Invalid(errors) => fail(s"Unable to build wom version of scatter foo from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }

    def validateGraph(workflowGraph: Graph) = {

      // Three inputs, a scatter node and two outputs:
      workflowGraph.nodes.size should be(6)

      // Find that scatter:
      workflowGraph.nodes.collectFirst {
        case s: ScatterNode => s
      }.getOrElse(fail("Resulting graph did not contain a ScatterNode"))


    }
  }

  it should "convert unsatisfied call inputs in scatters into outer graph inputs" in {
    val scatterTest =
      """task foo {
        |  Int i
        |  Int j
        |  command { ... }
        |  output {
        |    String str_out = i
        |    Int int_out = i + 1
        |  }
        |}
        |
        |workflow scatter_test {
        |  Int x = 5
        |  scatter (s in range(x)) {
        |    call foo { input: i = s } # nb: foo.j is missing!
        |  }
        |
        |  Array[String] str_outs = foo.str_out
        |}
        |""".stripMargin

    val namespace = WdlNamespace.loadUsingSource(scatterTest, None, None).get.asInstanceOf[WdlNamespaceWithWorkflow]
    import lenthall.validation.ErrorOr.ShortCircuitingFlatMap
    val scatterTestGraph = namespace.womExecutable.flatMap(_.graph)

    scatterTestGraph match {
      case Valid(g) => validateGraph(g)
      case Invalid(errors) => fail(s"Unable to build wom version of scatter foo from WDL: ${errors.toList.mkString("\n", "\n", "\n")}")
    }

    def validateGraph(workflowGraph: Graph) = {

      // Find the inputs:
      val inputNodes: Set[GraphInputNode] = workflowGraph.nodes.filterByType[GraphInputNode]
      inputNodes.map {_.name} should be(Set("x", "foo.j"))

      // Find that scatter:
      val scatterNode = workflowGraph.nodes.collectFirst {
        case s: ScatterNode => s
      }.getOrElse(fail("Resulting graph did not contain a ScatterNode"))

      val scatterInnerInputs: Set[GraphInputNode] = scatterNode.innerGraph.nodes.filterByType[GraphInputNode]
      scatterInnerInputs map {_.name} should be(Set("s", "foo.j"))
      (scatterInnerInputs.find(_.name == "foo.j").get, inputNodes.find(_.name == "foo.j").get) match {
        case (fooj1, fooj2) if fooj1 eq fooj2 => fail("Scatter has used the inner graph input Node as an outer graph input")
        case _ => // fine
      }

      // Find the outputs:
      val outputNodes = workflowGraph.nodes.collect {
        case output: GraphOutputNode => output
      }
      outputNodes map { on => (on.name, on.womType) } should be(Set(("foo.int_out", WdlArrayType(WdlIntegerType)), ("foo.str_out", WdlArrayType(WdlStringType))))

    }
  }

}
