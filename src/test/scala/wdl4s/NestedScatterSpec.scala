package wdl4s

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

class NestedScatterSpec extends FlatSpec with Matchers {
  val namespace = WdlNamespaceWithWorkflow.load(SampleWdl.NestedScatterWdl.wdlSource())

  def getTask(name: String): Task = namespace.tasks.find(_.unqualifiedName == name).get
  def getCall(name: String): Call = namespace.workflow.calls.find(_.unqualifiedName == name).get
  def getScatter(index: Int): Scatter = namespace.resolve("w.$scatter_" + index).get.asInstanceOf[Scatter]

  val taskA = getTask("A")
  val taskB = getTask("B")
  val taskC = getTask("C")
  val taskD = getTask("D")
  val taskE = getTask("E")

  val workflow = namespace.workflow

  val callA = getCall("A")
  val callB = getCall("B")
  val callC = getCall("C")
  val callD = getCall("D")
  val callE = getCall("E")
  val callF = getCall("F")
  val callG = getCall("G")
  val callH = getCall("H")

  val declCallB = callB.declarations.head
  val declCallC = callC.declarations.head
  val declCallD = callD.declarations.head

  val declTaskB = taskB.declarations.head
  val declTaskC = taskC.declarations.head
  val declTaskD = taskD.declarations.head

  val scatter0 = getScatter(0)
  val scatter3 = getScatter(3)
  val scatter1 = getScatter(1)
  val scatter2 = getScatter(2)

  it should "have 5 tasks" in {
    namespace.tasks shouldEqual Seq(taskA, taskB, taskC, taskD, taskE)
  }

  it should "have 0 imported WdlNamespaces" in {
    namespace.namespaces shouldEqual Seq()
  }

  val fqnTable = Table(
    ("fqn", "fqnWithAllScopes", "scope"),
    ("w", "w", workflow),
    ("A", "A", taskA),
    ("B", "B", taskB),
    ("C", "C", taskC),
    ("D", "D", taskD),
    ("E", "E", taskE),
    ("w.A", "w.A", callA),
    ("w.B", "w.$scatter_0.B", callB),
    ("w.C", "w.$scatter_0.C", callC),
    ("w.D", "w.D", callD),
    ("w.E", "w.$scatter_0.E", callE),
    ("w.F", "w.$scatter_3.F", callF),
    ("w.G", "w.$scatter_0.$scatter_1.G", callG),
    ("w.H", "w.$scatter_0.$scatter_2.H", callH),
    ("w.$scatter_0", "w.$scatter_0", scatter0),
    ("w.$scatter_3", "w.$scatter_3", scatter3),
    ("w.$scatter_1", "w.$scatter_0.$scatter_1", scatter1),
    ("w.$scatter_2", "w.$scatter_0.$scatter_2", scatter2),
    ("w.B.B_in", "w.$scatter_0.B.B_in", declCallB),
    ("w.C.C_in", "w.$scatter_0.C.C_in", declCallC),
    ("w.D.D_in", "w.D.D_in", declCallD),
    ("B.B_in", "B.B_in", declTaskB),
    ("C.C_in", "C.C_in", declTaskC),
    ("D.D_in", "D.D_in", declTaskD)
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
    (callA, Set(), Set(scatter0, scatter3)),
    (callB, Set(scatter0), Set(callC, scatter1, scatter2, callD)),
    (callC, Set(callB, scatter0), Set()),
    (callD, Set(callB), Set()),
    (callE, Set(scatter0), Set()),
    (callF, Set(scatter3), Set()),
    (callG, Set(scatter1, scatter0), Set()),
    (callH, Set(scatter2, scatter0), Set()),
    (scatter0, Set(callA), Set(callB, callC, callE, callG, callH, scatter1, scatter2)),
    (scatter3, Set(callA), Set(callF)),
    (scatter1, Set(callB, scatter0), Set(callG)),
    (scatter2, Set(callB, scatter0), Set(callH))
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
    (callA, Option(workflow), Seq()),
    (callB, Option(scatter0), Seq(declCallB)),
    (callC, Option(scatter0), Seq(declCallC)),
    (callD, Option(workflow), Seq(declCallD)),
    (callE, Option(scatter0), Seq()),
    (callF, Option(scatter3), Seq()),
    (callG, Option(scatter1), Seq()),
    (callH, Option(scatter2), Seq()),
    (workflow, Option(namespace), Seq(callA, scatter0, scatter3, callD)),
    (namespace, None, Seq(taskA, taskB, taskC, taskD, taskE, workflow))
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
    (taskB, Seq(namespace)),
    (taskC, Seq(namespace)),
    (taskD, Seq(namespace)),
    (taskE, Seq(namespace)),
    (declTaskB, Seq(taskB, namespace)),
    (declTaskC, Seq(taskC, namespace)),
    (declTaskD, Seq(taskD, namespace)),
    (callA, Seq(workflow, namespace)),
    (callB, Seq(scatter0, workflow, namespace)),
    (callC, Seq(scatter0, workflow, namespace)),
    (callD, Seq(workflow, namespace)),
    (callE, Seq(scatter0, workflow, namespace)),
    (callF, Seq(scatter3, workflow, namespace)),
    (callG, Seq(scatter1, scatter0, workflow, namespace)),
    (callH, Seq(scatter2, scatter0, workflow, namespace)),
    (scatter0, Seq(workflow, namespace)),
    (scatter1, Seq(scatter0, workflow, namespace)),
    (scatter2, Seq(scatter0, workflow, namespace)),
    (scatter3, Seq(workflow, namespace)),
    (declCallB, Seq(callB, scatter0, workflow, namespace)),
    (declCallC, Seq(callC, scatter0, workflow, namespace)),
    (declCallD, Seq(callD, workflow, namespace))
  )

  forAll(ancestryTable) { (node, ancestry) =>
    it should s"compute ancestry for ${node.fullyQualifiedName}" in {
      node.ancestry shouldEqual ancestry
    }
  }


  it should "Have four 'children' objects" in {
    namespace.workflow.children.size shouldEqual 4
  }

  it should "Have two 'direct Call descendents' objects" in {
    namespace.workflow.children.collect({ case c: Call => c }).size shouldEqual 2
  }

  it should "Have two 'direct Scatter descendents' objects" in {
    namespace.workflow.children.collect({ case s: Scatter => s }).size shouldEqual 2
  }

  it should "Have eight 'Call' objects" in {
    namespace.workflow.calls.size shouldEqual 8
  }

  it should "Have four 'Scatter' objects" in {
    namespace.workflow.scatters.size shouldEqual 4
  }

  it should "Have 'Scatter' objects indexed properly" in {
    val scatters = namespace.workflow.scatters
    scatters.find(_.fullyQualifiedName  == "w.$scatter_0").map(_.index) shouldEqual Option(0)
    scatters.find(_.fullyQualifiedName  == "w.$scatter_1").map(_.index) shouldEqual Option(1)
    scatters.find(_.fullyQualifiedName  == "w.$scatter_2").map(_.index) shouldEqual Option(2)
    scatters.find(_.fullyQualifiedName  == "w.$scatter_3").map(_.index) shouldEqual Option(3)
  }

  it should "Not appear in Calls FQNs" in {
    val calls = namespace.workflow.calls
    calls.find(_.fullyQualifiedName == "w.A") shouldBe defined
    calls.find(_.fullyQualifiedName == "w.B") shouldBe defined
    calls.find(_.fullyQualifiedName == "w.C") shouldBe defined
    calls.find(_.fullyQualifiedName == "w.E") shouldBe defined
    calls.find(_.fullyQualifiedName == "w.G") shouldBe defined
    calls.find(_.fullyQualifiedName == "w.H") shouldBe defined
    calls.find(_.fullyQualifiedName == "w.F") shouldBe defined
    calls.find(_.fullyQualifiedName == "w.D") shouldBe defined
  }

  it should "Have correct FQNs for Scatter blocks" in {
    namespace.workflow.scatters.find(_.fullyQualifiedNameWithIndexScopes == "w.$scatter_0") shouldBe defined
    namespace.workflow.scatters.find(_.fullyQualifiedNameWithIndexScopes == "w.$scatter_0.$scatter_1") shouldBe defined
    namespace.workflow.scatters.find(_.fullyQualifiedNameWithIndexScopes == "w.$scatter_0.$scatter_2") shouldBe defined
    namespace.workflow.scatters.find(_.fullyQualifiedNameWithIndexScopes == "w.$scatter_3") shouldBe defined
  }

  it should "Instantiate Scatters with correct item attributes" in {
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_0").get.item shouldEqual "item"
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_1").get.item shouldEqual "itemB"
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_2").get.item shouldEqual "itemB"
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_3").get.item shouldEqual "item"
  }

  it should "Instantiate Scatters with correct collection attributes" in {
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_0").get.collection.toWdlString shouldEqual "A.A_out"
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_1").get.collection.toWdlString shouldEqual "B.B_out"
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_2").get.collection.toWdlString shouldEqual "B.B_out"
    namespace.workflow.scatters.find(_.unqualifiedName == "$scatter_3").get.collection.toWdlString shouldEqual "A.A_out"
  }

}
