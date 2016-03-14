package wdl4s

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

class IfStatementSpec extends FlatSpec with Matchers with WdlTest {
  val namespace = WdlNamespaceWithWorkflow.load(
    """task A {
      |  String i
      |  command {ps}
      |  output {String o = read_string(stdout())}
      |}
      |
      |workflow w {
      |  Int i
      |  Array[String] arr
      |
      |  call A
      |
      |  if (i == 2) {
      |    call A as B
      |  }
      |
      |  if (A.o == "foo") {
      |    call A as C
      |  }
      |
      |  if (A.o == "bar") {
      |    scatter(x in arr) {
      |      call A as D {input: i=x}
      |    }
      |  }
      |
      |  call A as E {input: i=C.o}
      |}
      |
    """.stripMargin)

  val taskA = getTask("A")

  val callA = getCall("A")
  val callB = getCall("B")
  val callC = getCall("C")
  val callD = getCall("D")
  val callE = getCall("E")

  val declI = workflow.declarations.find(_.unqualifiedName == "i").get
  val declArr = workflow.declarations.find(_.unqualifiedName == "arr").get

  val callAIn = callA.declarations.head
  val callBIn = callB.declarations.head
  val callCIn = callC.declarations.head
  val callDIn = callD.declarations.head
  val callEIn = callE.declarations.head

  val taskAIn = taskA.declarations.head
  val taskAOut = taskA.outputs.head

  val scatter0 = getScatter(0)
  val if0 = getIf(0)
  val if1 = getIf(1)
  val if2 = getIf(2)

  it should "have 1 task" in {
    namespace.tasks shouldEqual Seq(taskA)
  }

  it should "have 0 imported WdlNamespaces" in {
    namespace.namespaces shouldEqual Seq()
  }

  val fqnTable = Table(
    ("fqn", "fqnWithAllScopes", "scope"),
    ("w", "w", workflow),
    ("A", "A", taskA),
    ("w.A", "w.A", callA),
    ("w.B", "w.$if_0.B", callB),
    ("w.C", "w.$if_1.C", callC),
    ("w.D", "w.$if_2.$scatter_0.D", callD),
    ("w.E", "w.E", callE),
    ("w.$scatter_0", "w.$if_2.$scatter_0", scatter0),
    ("w.$if_0", "w.$if_0", if0),
    ("w.$if_1", "w.$if_1", if1),
    ("w.$if_2", "w.$if_2", if2),
    ("w.A.i", "w.A.i", callAIn),
    ("w.B.i", "w.$if_0.B.i", callBIn),
    ("w.C.i", "w.$if_1.C.i", callCIn),
    ("w.D.i", "w.$if_2.$scatter_0.D.i", callDIn),
    ("w.E.i", "w.E.i", callEIn)
  )

  forAll(fqnTable) { (fqn, fqnWithAllScopes, scope) =>
    it should s"resolve FQN: $fqn" in {
      namespace.resolve(fqn) shouldEqual Option(scope)
    }

    it should s"resolve FQN (with scopes): $fqnWithAllScopes" in {
      namespace.resolve(fqnWithAllScopes) shouldEqual Option(scope)
    }

    it should s"generate FQN: $fqn" in {
      scope.fullyQualifiedName shouldEqual fqn
    }

    it should s"generate FQN (with scopes): $fqn" in {
      scope.fullyQualifiedNameWithIndexScopes shouldEqual fqnWithAllScopes
    }
  }

  val dependencyTable = Table(
    ("node", "upstream", "downstream"),
    (callA, Set(), Set(if1, if2)),
    (callB, Set(if0), Set()),
    (callC, Set(if1), Set(callE)),
    (callD, Set(scatter0), Set()),
    (callE, Set(callC), Set()),
    (scatter0, Set(if2, declArr), Set(callD)),
    (if0, Set(declI), Set(callB)),
    (if1, Set(callA), Set(callC)),
    (if2, Set(callA), Set(scatter0)),
    (callAIn, Set(), Set()),
    (callBIn, Set(), Set()),
    (callCIn, Set(), Set()),
    (callDIn, Set(), Set()),
    (callEIn, Set(), Set())
  )

  forAll(dependencyTable) { (node, upstream, downstream) =>
    it should s"compute upstream nodes for ${node.fullyQualifiedName}" in {
      node.upstream shouldEqual upstream
    }
    it should s"compute downstream nodes for ${node.fullyQualifiedName}" in {
      node.downstream shouldEqual downstream
    }
  }

  val parentAndChildrenTable = Table(
    ("node", "parent", "children"),
    (callA, Option(workflow), Seq(callAIn)),
    (callB, Option(if0), Seq(callBIn)),
    (callC, Option(if1), Seq(callCIn)),
    (callD, Option(scatter0), Seq(callDIn)),
    (callE, Option(workflow), Seq(callEIn)),
    (workflow, Option(namespace), Seq(declI, declArr, callA, if0, if1, if2, callE)),
    (namespace, None, Seq(taskA, workflow)),
    (declI, Option(workflow), Seq()),
    (declArr, Option(workflow), Seq())
  )

  forAll(parentAndChildrenTable) { (node, parent, children) =>
    it should s"compute children for ${node.fullyQualifiedName}" in {
      node.children shouldEqual children
    }
    it should s"compute parents for ${node.fullyQualifiedName}" in {
      node.parent shouldEqual parent
    }
  }

  val ancestryTable = Table(
    ("node", "ancestry"),
    (namespace, Seq()),
    (workflow, Seq(namespace)),
    (taskA, Seq(namespace)),
    (taskAIn, Seq(taskA, namespace)),
    (callA, Seq(workflow, namespace)),
    (callB, Seq(if0, workflow, namespace)),
    (callC, Seq(if1, workflow, namespace)),
    (callD, Seq(scatter0, if2, workflow, namespace)),
    (callE, Seq(workflow, namespace)),
    (scatter0, Seq(if2, workflow, namespace)),
    (if0, Seq(workflow, namespace)),
    (if1, Seq(workflow, namespace)),
    (if2, Seq(workflow, namespace)),
    (callAIn, Seq(callA, workflow, namespace)),
    (callBIn, Seq(callB, if0, workflow, namespace)),
    (callCIn, Seq(callC, if1, workflow, namespace)),
    (callDIn, Seq(callD, scatter0, if2, workflow, namespace)),
    (callEIn, Seq(callE, workflow, namespace))
  )

  forAll(ancestryTable) { (node, ancestry) =>
    it should s"compute ancestry for ${node.fullyQualifiedName}" in {
      node.ancestry shouldEqual ancestry
    }
  }

  val scatterTable = Table(
    ("node", "item", "collection", "index"),
    (scatter0, "x", "arr", 0)
  )

  forAll(scatterTable) { (node, item, collection, index) =>
    it should s"have '$item' as the iteration variable for scatter block ${node.fullyQualifiedName}" in {
      node.item shouldEqual item
    }
    it should s"have '$collection' as the expression for scatter block ${node.fullyQualifiedName}" in {
      node.collection.toWdlString shouldEqual collection
    }
    it should s"have '$index' as the index scatter block ${node.fullyQualifiedName}" in {
      node.index shouldEqual index
    }
  }

  val ifTable = Table(
    ("node", "expression", "index"),
    (if0, "i == 2", 0),
    (if1, "A.o == \"foo\"", 1),
    (if2, "A.o == \"bar\"", 2)
  )

  forAll(ifTable) { (node, expression, index) =>
    it should s"have a correct condition expression for if block ${node.fullyQualifiedName}" in {
      node.condition.toWdlString shouldEqual expression
    }
    it should s"have a correct index for if block ${node.fullyQualifiedName}" in {
      node.index shouldEqual index
    }
  }

  val lookupVarTable = Table(
    ("node", "variable", "resolution"),
    (callD, "x", Some(scatter0)),
    (callE, "D", Some(callD))
  )

  forAll(lookupVarTable) { (node, variable, resolution) =>
    it should s"resolve variable $variable (relative to ${node.fullyQualifiedName}) -> ${resolution.map(_.fullyQualifiedName).getOrElse("None")}" in {
      node.resolveVariable(variable) shouldEqual resolution
    }
  }

  it should "Have five 'Call' objects" in {
    namespace.workflow.calls shouldEqual Set(callA, callB, callC, callD, callE)
  }

  it should "Have one 'Scatter' objects" in {
    namespace.workflow.scatters shouldEqual Set(scatter0)
  }
}
