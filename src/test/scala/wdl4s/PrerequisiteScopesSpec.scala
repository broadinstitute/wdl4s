package wdl4s

import wdl4s.SampleWdl.ScatterWdl
import org.scalatest.{FlatSpec, Matchers}

class PrerequisiteScopesSpec extends FlatSpec with Matchers {
  val namespace = NamespaceWithWorkflow.load((new ScatterWdl).wdlSource())
  val workflow = namespace.workflow
  val allCalls = workflow.calls
  val allScatters = workflow.scatters

  def scopesByName(name: String) = workflow.callByName(name).get.prerequisiteScopes

  "ScatterWdl" should "have five calls" in {
    allCalls.size shouldEqual 5
  }

  it should "have one scatter block" in {
    allScatters.size shouldEqual 1
  }

  it should "have the scatter block with one prereq" in {
    allScatters.head.prerequisiteScopes.size shouldEqual 1
  }

  it should "have the scatter block with A as a prereq" in {
    val z = allScatters.head.prerequisiteScopes.head.fullyQualifiedName shouldEqual "w.A"
  }

  it should "have A not depend on anything" in {
    scopesByName("A") shouldBe empty
  }

  it should "have B depend on the scatter" in {
    val scopes = scopesByName("B")
    scopes.size shouldEqual 1
    scopes.head.unqualifiedName shouldBe "$scatter_0"
  }

  it should "have C depend on the scatter and B" in {
    val scopes = scopesByName("C")
    scopes.size shouldEqual 2
    scopes find { _.unqualifiedName == "$scatter_0" } shouldBe defined
    scopes find { _.unqualifiedName == "B"} shouldBe defined
  }

  it should "have D depend on B" in {
    val scopes = scopesByName("D")
    scopes.size shouldEqual 1
    scopes.head.unqualifiedName shouldBe "B"
  }

  it should "have E depend on the scatter" in {
    val scopes = scopesByName("E")
    scopes.size shouldEqual 1
    scopes.head.unqualifiedName shouldBe "$scatter_0"
  }
}
