package wdl4s

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table

class ThreeStepImportNamespaceAliasSpec extends FlatSpec with Matchers {
  val psTaskWdl = """
    |task ps {
    |  command {
    |    ps
    |  }
    |  output {
    |    File procs = stdout()
    |  }
    |}""".stripMargin

  val cgrepTaskWdl = """
    |task cgrep {
    |  String pattern
    |  File in_file
    |  command {
    |    grep '${pattern}' ${in_file} | wc -l
    |  }
    |  output {
    |    Int count = read_int(stdout())
    |  }
    |}""".stripMargin

  val wcTaskWdl = """
    |task wc {
    |  File in_file
    |  command {
    |    cat ${in_file} | wc -l
    |  }
    |  output {
    |    Int count = read_int(stdout())
    |  }
    |}""".stripMargin

  val workflowWdl = """
    |import "ps" as ns1
    |import "cgrep" as ns2
    |import "wc" as ns3
    |
    |workflow three_step {
    |  call ns1.ps as a1
    |  call ns2.cgrep as a2 {
    |    input: in_file=a1.procs
    |  }
    |  call ns3.wc {
    |    input: in_file=a1.procs
    |  }
    |}""".stripMargin

  def resolver(importUri: String): WdlSource = {
    importUri match {
      case "ps" => psTaskWdl
      case "cgrep" => cgrepTaskWdl
      case "wc" => wcTaskWdl
      case _ => throw new RuntimeException(s"Can't resolve $importUri")
    }
  }

  val namespace = WdlNamespaceWithWorkflow.load(workflowWdl, resolver _)
  val workflow = namespace.workflow
  val allTasks = namespace.tasks ++ namespace.namespaces.flatMap(_.tasks)
  val allCalls = workflow.calls

  val callA1 = allCalls.find(_.unqualifiedName == "a1").get
  val callA2 = allCalls.find(_.unqualifiedName == "a2").get
  val callWc = allCalls.find(_.unqualifiedName == "wc").get
  val taskPs = allTasks.find(_.unqualifiedName == "ps").get
  val taskCgrep = allTasks.find(_.unqualifiedName == "cgrep").get
  val taskWc = allTasks.find(_.unqualifiedName == "wc").get
  val ns1 = namespace.namespaces.find(_.importedAs.contains("ns1")).get
  val ns2 = namespace.namespaces.find(_.importedAs.contains("ns2")).get
  val ns3 = namespace.namespaces.find(_.importedAs.contains("ns3")).get
  val declCallCgrepInfile = callA2.declarations.find(_.unqualifiedName == "in_file").get
  val declTaskCgrepInfile = taskCgrep.declarations.find(_.unqualifiedName == "in_file").get
  val declCallCgrepPattern = callA2.declarations.find(_.unqualifiedName == "pattern").get
  val declTaskCgrepPattern = taskCgrep.declarations.find(_.unqualifiedName == "pattern").get
  val declCallWcInfile = callWc.declarations.find(_.unqualifiedName == "in_file").get
  val declTaskWcInfile = taskWc.declarations.find(_.unqualifiedName == "in_file").get

  it should "have 0 tasks (3 tasks are in separate WdlNamespaces)" in {
    namespace.tasks.size shouldEqual 0
  }
  it should "have 3 imported WdlNamespaces with names ns1, ns2, and ns3" in {
    namespace.namespaces.map(_.importedAs.get).toSet shouldEqual Set("ns1", "ns2", "ns3")
  }

  it should "have 3 imported WdlNamespaces with tasks 'ps', 'cgrep', and 'wc'" in {
    namespace.namespaces.flatMap(_.tasks) shouldEqual Seq(taskPs, taskCgrep, taskWc)
    ns1.tasks shouldEqual Seq(taskPs)
    ns2.tasks shouldEqual Seq(taskCgrep)
    ns3.tasks shouldEqual Seq(taskWc)
  }

  val fqnTable = Table(
    ("fqn", "scope"),
    ("three_step", workflow),
    ("three_step.a1", callA1),
    ("three_step.a2", callA2),
    ("three_step.wc", callWc),
    ("three_step.a2.in_file", declCallCgrepInfile),
    ("three_step.a2.pattern", declCallCgrepPattern),
    ("three_step.wc.in_file", declCallWcInfile),
    ("ns1", ns1),
    ("ns2", ns2),
    ("ns3", ns3),
    ("ns1.ps", taskPs),
    ("ns2.cgrep", taskCgrep),
    ("ns2.cgrep.in_file", declTaskCgrepInfile),
    ("ns2.cgrep.pattern", declTaskCgrepPattern),
    ("ns3.wc", taskWc),
    ("ns3.wc.in_file", declTaskWcInfile)
  )

  forAll(fqnTable) { (fqn, scope) =>
    it should s"resolve FQN: $fqn" in {
      namespace.resolve(fqn) shouldEqual Option(scope)
    }

    it should s"generate FQN: $fqn" in {
      scope.fullyQualifiedName shouldEqual fqn
    }

    it should s"generate FQN (with scopes): $fqn" in {
      scope.fullyQualifiedNameWithIndexScopes shouldEqual fqn
    }
  }

  it should "compute upstream nodes" in {
    val upstreamTable = Table(
      ("node", "upstream"),
      (callA1, Set()),
      (callA2, Set(callA1)),
      (callWc, Set(callA1))
    )

    forAll(upstreamTable) { (node, upstream) =>
      node.upstream shouldEqual upstream
    }
  }

  it should "compute downstream nodes" in {
    val downstreamTable = Table(
      ("node", "downstream"),
      (callA1, Set(callA2, callWc)),
      (callA2, Set()),
      (callWc, Set())
    )

    forAll(downstreamTable) { (node, downstream) =>
      node.downstream shouldEqual downstream
    }
  }

  it should "compute ancestry" in {
    val ancestryTable = Table(
      ("node", "ancestry"),
      (callA1, Seq(workflow, namespace)),
      (callA2, Seq(workflow, namespace)),
      (callWc, Seq(workflow, namespace)),
      (workflow, Seq(namespace)),
      (namespace, Seq()),
      (ns1, Seq(namespace)),
      (ns2, Seq(namespace)),
      (ns3, Seq(namespace)),
      (declCallCgrepInfile, Seq(callA2, workflow, namespace)),
      (declCallCgrepPattern, Seq(callA2, workflow, namespace)),
      (declCallWcInfile, Seq(callWc, workflow, namespace)),
      (declTaskCgrepInfile, Seq(taskCgrep, ns2, namespace)),
      (declTaskCgrepPattern, Seq(taskCgrep, ns2, namespace)),
      (declTaskWcInfile, Seq(taskWc, ns3, namespace))
    )

    forAll(ancestryTable) { (node, ancestry) =>
      node.ancestry shouldEqual ancestry
    }
  }

  it should "compute parents" in {
    val parentTable = Table(
      ("node", "parent"),
      (callA1, Option(workflow)),
      (callA2, Option(workflow)),
      (callWc, Option(workflow)),
      (workflow, Option(namespace)),
      (namespace, None),
      (ns1, Option(namespace)),
      (ns2, Option(namespace)),
      (ns3, Option(namespace)),
      (declCallCgrepInfile, Option(callA2)),
      (declCallCgrepPattern, Option(callA2)),
      (declCallWcInfile, Option(callWc)),
      (declTaskCgrepInfile, Option(taskCgrep)),
      (declTaskCgrepPattern, Option(taskCgrep)),
      (declTaskWcInfile, Option(taskWc)),
      (namespace, None)
    )

    forAll(parentTable) { (node, parent) =>
      node.parent shouldEqual parent
    }
  }

  it should "compute children" in {
    val childrenTable = Table(
      ("node", "children"),
      (callA1, Seq()),
      (callA2, Seq(declCallCgrepPattern, declCallCgrepInfile)),
      (callWc, Seq(declCallWcInfile)),
      (workflow, Seq(callA1, callA2, callWc)),
      (namespace, Seq(ns1, ns2, ns3, workflow)),
      (ns1, Seq(taskPs)),
      (ns2, Seq(taskCgrep)),
      (ns3, Seq(taskWc)),
      (declCallCgrepInfile, Seq()),
      (declCallCgrepPattern, Seq()),
      (declCallWcInfile, Seq()),
      (declTaskCgrepInfile, Seq()),
      (declTaskCgrepPattern, Seq()),
      (declTaskWcInfile, Seq())
    )

    forAll(childrenTable) { (node, children) =>
      node.children shouldEqual children
    }
  }

  it should "compute namespace" in {
    val namespaceTable = Table(
      ("node", "namespace"),
      (callA1, namespace),
      (callA2, namespace),
      (callWc, namespace),
      (workflow, namespace),
      (namespace, namespace),
      (ns1, ns1),
      (ns2, ns2),
      (ns3, ns3),
      (declCallCgrepInfile, namespace),
      (declCallCgrepPattern, namespace),
      (declCallWcInfile, namespace),
      (declTaskCgrepInfile, ns2),
      (declTaskCgrepPattern, ns2),
      (declTaskWcInfile, ns3)
    )

    forAll(namespaceTable) { (node, namespace) =>
      node.namespace shouldEqual namespace
    }
  }

  it should "throw an exception if the import resolver fails to resolve an import" in {
    try {
      WdlNamespace.load(workflowWdl, (s: String) => throw new NotImplementedError("not implemented"))
      fail("Expecting an exception to be thrown")
    } catch {
      case _: NotImplementedError =>
    }
  }
}

