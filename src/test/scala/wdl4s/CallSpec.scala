package wdl4s

import org.scalatest.{Matchers, WordSpec}
import wdl4s.exception.ValidationException
import wdl4s.expression.{NoFunctions, PureStandardLibraryFunctionsLike}
import wdl4s.types.{WdlArrayType, WdlFileType, WdlIntegerType, WdlStringType}
import wdl4s.values._

import scala.util.{Failure, Success, Try}

class CallSpec extends WordSpec with Matchers {

  "evaluate its declarations" in {
    val namespace = WdlNamespaceWithWorkflow.load(SampleWdl.TaskDeclarationsWdl.wdlSource())
    val callT = namespace.calls.find(_.unqualifiedName == "t").get
    val callT2 = namespace.calls.find(_.unqualifiedName == "t2").get
    val callV = namespace.calls.find(_.unqualifiedName == "v").get

    val inputs = namespace.coerceRawInputs(SampleWdl.TaskDeclarationsWdl.rawInputs).get
    
    def outputResolver(call: GraphNode, index: Option[Int]): Try[WdlValue] = {
      (call, index) match {
        case (c, Some(2)) if c == callT => Success(WdlCallOutputsObject(callT, Map("o" -> WdlString(s"c ${index.getOrElse(-1)}"))))
        case (c, None) if c == callT2 => Success(WdlCallOutputsObject(callT2, Map(
          "outputArray" -> WdlArray(WdlArrayType(WdlIntegerType), Seq(WdlInteger(0), WdlInteger(1), WdlInteger(2)))
        )))
        case _ => Failure(new Exception(s"no output found for call ${call.fullyQualifiedName}"))
      }
    }
    
    val shardMap = Map(namespace.scatters.head -> 2)

    val declarations = callV.evaluateTaskInputs(inputs, NoFunctions, outputResolver, shardMap)
    declarations.size shouldBe 11
    declarations.find(_._1.unqualifiedName == "a").get._2 shouldBe WdlString("a")
    declarations.find(_._1.unqualifiedName == "b").get._2 shouldBe WdlString("b")
    declarations.find(_._1.unqualifiedName == "c").get._2 shouldBe WdlString("c 2")
    declarations.find(_._1.unqualifiedName == "d").get._2 shouldBe WdlInteger(2)
    declarations.find(_._1.unqualifiedName == "e").get._2 shouldBe WdlString("e")
    declarations.find(_._1.unqualifiedName == "f").get._2 shouldBe WdlString("f")
    declarations.find(_._1.unqualifiedName == "g").get._2 shouldBe WdlOptionalValue(WdlString("g"))
    declarations.find(_._1.unqualifiedName == "h").get._2 shouldBe WdlOptionalValue(WdlStringType, None)
    declarations.find(_._1.unqualifiedName == "i").get._2 shouldBe WdlString("b")
    declarations.find(_._1.unqualifiedName == "j").get._2 shouldBe WdlFile("j")
    declarations.find(_._1.unqualifiedName == "k").get._2 shouldBe WdlArray(WdlArrayType(WdlFileType), Seq(WdlFile("a"), WdlFile("b"), WdlFile("c")))
  }
  
  "accumulate input evaluation errors and throw an exception" in {
    val wdl =
      """
        |task t {
        | String s1
        | String s2
        | command {...}
        |}
        |
        |workflow w {
        | call t
        |}
      """.stripMargin
    
    val namespace = WdlNamespace.loadUsingSource(wdl, None, None)
    val callT = namespace.calls.find(_.unqualifiedName == "t").get
    val exception = the[ValidationException] thrownBy {
      callT.evaluateTaskInputs(Map.empty, NoFunctions)
    }
    exception.getMessage shouldBe "Input evaluation for Call w.t failed.\ns1: Could not find s1 in input section of call w.t\ns2: Could not find s2 in input section of call w.t"
  }
  
  "find workflows" in {

    val subWorkflow =
      """
        |task hello2 {
        |  String addressee = "hey"
        |  command {
        |    echo "Hello ${addressee}!"
        |  }
        |  output {
        |    String salutation = read_string(stdout())
        |  }
        |}
        |
        |workflow sub_hello {
        |  call hello2
        |  output {
        |    String result = hello2.salutation
        |  }
        |}
      """.stripMargin

    val wdl =
      s"""
        |import "placeholder" as sub
        |
        |task hello {
        |  String addressee
        |  command {
        |    echo "Hello $${addressee}!"
        |  }
        |  output {
        |    String salutation = read_string(stdout())
        |  }
        |}
        |
        |workflow wf_hello {
        |  call sub.sub_hello
        |  call hello {input: addressee = sub_hello.result }
        |}
      """.stripMargin

    val ns = WdlNamespaceWithWorkflow.load(wdl, importResolver = (uri: String) => subWorkflow)
    ns.workflow.workflowCalls.size shouldBe 1
    ns.workflow.taskCalls.size shouldBe 1
  }
  
  "bubble up evaluation exception" in {
    val task =
      """
        |task hello {
        |  String addressee
        |  command {
        |    echo "Hello ${addressee}!"
        |  }
        |  runtime {
        |      docker: "ubuntu:latest"
        |    }
        |  output {
        |    String salutation = read_string(stdout())
        |  }
        |}""".stripMargin
    
      val workflow = """
        |workflow wf_hello {
        |  File wf_hello_input
        |  
        |  call hello {input: addressee = read_string(wf_hello_input) }
        |  
        |  output {
        |    String salutation = hello.salutation
        |  }
        |}
      """.stripMargin
    
    
    val functionsWithRead = new PureStandardLibraryFunctionsLike {
      override def readFile(path: String): String = {
        import better.files._
        File(path).contentAsString
      }
    }

    val ns = WdlNamespaceWithWorkflow.load(workflow + task, Seq.empty)
    val exception = intercept[ValidationException] {
        ns.workflow.calls.head.evaluateTaskInputs(Map("wf_hello.wf_hello_input" -> WdlFile("/do/not/exist")), functionsWithRead)
    }
    
    exception.getMessage shouldBe "Input evaluation for Call wf_hello.hello failed.\naddressee: File not found /do/not/exist"

    val workflow2 = """
     |workflow wf_hello {
     |  File wf_hello_input
     |  String read = read_string(wf_hello_input)
     |  
     |  call hello {input: addressee = read }
     |  
     |  output {
     |    String salutation = hello.salutation
     |  }
     |}
   """.stripMargin

    val ns2 = WdlNamespaceWithWorkflow.load(workflow2 + task, Seq.empty)
    val exception2 = intercept[ValidationException] {
      ns.workflow.calls.head.evaluateTaskInputs(Map("wf_hello.wf_hello_input" -> WdlFile("/do/not/exist")), functionsWithRead)
    }
    
    exception2.getMessage shouldBe "Input evaluation for Call wf_hello.hello failed.\naddressee: File not found /do/not/exist"
  }

}
