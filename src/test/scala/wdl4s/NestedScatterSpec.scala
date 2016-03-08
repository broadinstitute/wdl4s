package wdl4s

import org.scalatest.{FlatSpec, Matchers}

class NestedScatterSpec extends FlatSpec with Matchers {
  val namespace = WdlNamespaceWithWorkflow.load(SampleWdl.NestedScatterWdl.wdlSource())

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
