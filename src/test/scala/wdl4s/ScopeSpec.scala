package wdl4s

import wdl4s.AstTools.AstNodeName
import wdl4s.parser.WdlParser.Ast
import org.scalatest.{FlatSpec, Matchers}

class ScopeSpec extends FlatSpec with Matchers {

  val namespace = WdlNamespaceWithWorkflow.load(SampleWdl.NestedScatterWdl.wdlSource())
  val calls: Set[Call] = namespace.workflow.calls
  val scatters: Set[Scatter] = namespace.workflow.scatters
  val scatter0: Scatter = scatters.find(_.unqualifiedName == "$scatter_0").get
  val scatter1: Scatter = scatters.find(_.unqualifiedName == "$scatter_1").get
  val scatter2: Scatter = scatters.find(_.unqualifiedName == "$scatter_2").get
  val scatter3: Scatter = scatters.find(_.unqualifiedName == "$scatter_3").get
  val callA: Call = calls.find(_.fullyQualifiedName == "w.A").get
  val callB: Call = calls.find(_.fullyQualifiedName == "w.B").get
  val callC: Call = calls.find(_.fullyQualifiedName == "w.C").get
  val callD: Call = calls.find(_.fullyQualifiedName == "w.D").get
  val callE: Call = calls.find(_.fullyQualifiedName == "w.E").get
  val callF: Call = calls.find(_.fullyQualifiedName == "w.F").get
  val callG: Call = calls.find(_.fullyQualifiedName == "w.G").get
  val callH: Call = calls.find(_.fullyQualifiedName == "w.H").get

  it should "Have correct parent hierarchy" in {
    callA.parent.get shouldEqual namespace.workflow
    callB.parent.get shouldEqual scatter0
    callC.parent.get shouldEqual scatter0
    callE.parent.get shouldEqual scatter0
    callG.parent.get shouldEqual scatter1
    callH.parent.get shouldEqual scatter2
    callF.parent.get shouldEqual scatter3
    callD.parent.get shouldEqual namespace.workflow

    scatter0.parent.get shouldEqual namespace.workflow
    scatter1.parent.get shouldEqual scatter0
    scatter2.parent.get shouldEqual scatter0
    scatter3.parent.get shouldEqual namespace.workflow

    namespace.workflow.parent shouldEqual Option(namespace)
    namespace.parent shouldEqual None
  }

  it should "Have correct parent/child relationships" in {
    namespace.workflow.children shouldEqual Seq(callA, scatter0, scatter3, callD)

    scatter0.children shouldEqual Seq(callB, callC, callE, scatter1, scatter2)
    scatter1.children shouldEqual Seq(callG)
    scatter2.children shouldEqual Seq(callH)
    scatter3.children shouldEqual Seq(callF)

    val B_in = namespace.resolve("w.B.B_in").get
    val C_in = namespace.resolve("w.C.C_in").get
    val D_in = namespace.resolve("w.D.D_in").get

    callA.children shouldBe empty
    callB.children shouldBe Seq(B_in)
    callC.children shouldBe Seq(C_in)
    callD.children shouldBe Seq(D_in)
    callE.children shouldBe empty
    callF.children shouldBe empty
    callG.children shouldBe empty
    callH.children shouldBe empty
  }

  it should "Have correct ancestry for each Scope" in {
    callA.ancestry shouldEqual Seq(namespace.workflow, namespace)
    callB.ancestry shouldEqual Seq(scatter0, namespace.workflow, namespace)
    callC.ancestry shouldEqual Seq(scatter0, namespace.workflow, namespace)
    callE.ancestry shouldEqual Seq(scatter0, namespace.workflow, namespace)
    callG.ancestry shouldEqual Seq(scatter1, scatter0, namespace.workflow, namespace)
    callH.ancestry shouldEqual Seq(scatter2, scatter0, namespace.workflow, namespace)
    callF.ancestry shouldEqual Seq(scatter3, namespace.workflow, namespace)
    callD.ancestry shouldEqual Seq(namespace.workflow, namespace)
  }

  it should "Be able to determine common ancestor between two Scopes" in {
    callA.closestCommonAncestor(callH) shouldEqual Some(namespace.workflow)
    callH.closestCommonAncestor(callA) shouldEqual Some(namespace.workflow)
    callB.closestCommonAncestor(callC) shouldEqual Some(scatter0)
    callC.closestCommonAncestor(callB) shouldEqual Some(scatter0)
    callG.closestCommonAncestor(callH) shouldEqual Some(scatter0)
  }

  it should "throw an exception if trying to re-assign children on a scope" in {
    the [UnsupportedOperationException] thrownBy { namespace.workflow.children = Seq.empty } should have message "children is write-once"
  }

  it should "throw an exception if trying to generate a workflow from a non-workflow ast" in {
    val callAst: Ast = AstTools.findAsts(namespace.ast, AstNodeName.Call).head
    the [UnsupportedOperationException] thrownBy {
      Workflow(callAst, namespace.wdlSyntaxErrorFormatter)
    } should have message "Expecting Workflow AST, got a Call AST"
  }
}
