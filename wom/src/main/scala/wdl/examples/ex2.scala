package wdl.examples

import wdl.{WorkflowSource, WdlNamespaceWithWorkflow}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object ex2 {
  def main(args: Array[String]): Unit = {
    val wdl = """
      |import "some_string"
      |task a {
      |  command { ps }
      |}
      |workflow wf {
      | call a
      |}""".stripMargin

    def resolver(importString: String): Future[WorkflowSource] = Future {
      importString match {
        case "some_string" => "task imported { command {ps} }"
        case s if s.startsWith("http://") =>
          // issue HTTP request
          throw new NotImplementedError("not implemented")
      }
    }

    val ns = WdlNamespaceWithWorkflow.load(wdl, Seq(resolver _)).get

    ns.tasks foreach {task =>
      println(s"Task: ${task.name}")
    }
  }
}
