package wdl4s

import better.files.File
import wdl4s.parser.WdlParser.SyntaxError
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import wdl4s.util.StringUtil

class SyntaxErrorSpec extends FlatSpec with Matchers {
  private val psTaskWdl = """
      |task ps {
      |  command {
      |    ps
      |  }
      |  output {
      |    File procs = stdout()
      |  }
      |}""".stripMargin

  private val cgrepTaskWdl = """
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

  private def resolver(importUri: String): WdlSource = {
    importUri match {
      case "ps" => psTaskWdl
      case "cgrep" => cgrepTaskWdl
      case _ => throw new RuntimeException(s"Can't resolve $importUri")
    }
  }

  private def normalizeErrorMessage(msg: String) = StringUtil.stripAll(msg, " \t\n\r", " \t\n\r").replaceAll("[ \t]+\n", "\n")

  trait ErrorWdl {
    def testString: String
    def wdl: WdlSource
    def errors: String
  }

  case object CallReferencesBadInput extends ErrorWdl {
    val testString = "detect when call input section references an input that doesn't exist on the corresponding task"
    val wdl =
      """import "ps"
        |import "cgrep"
        |
        |workflow three_step {
        |  call ps.ps
        |  call cgrep.cgrep {
        |    input: BADin_file=ps.procs
        |  }
        |}
      """.stripMargin

    val errors =
      """ERROR: Call references an input on task 'cgrep' that doesn't exist (line 7, col 12)
        |
        |    input: BADin_file=ps.procs
        |           ^
        |
        |Task defined here (line 2, col 6):
        |
        |task cgrep {
        |     ^
      """.stripMargin
  }

  case object CallReferencesBadTask extends ErrorWdl {
    val testString = "detect when call references a task that doesn't exist"
    val wdl =
      """import "ps"
        |import "cgrep"
        |
        |workflow three_step {
        |  call ps.ps
        |  call cgrep.cgrepBAD {
        |    input: in_file=ps.procs
        |  }
        |}
      """.stripMargin

    val errors =
      """ERROR: Call references a task (cgrep.cgrepBAD) that doesn't exist (line 6, col 8)
        |
        |  call cgrep.cgrepBAD {
        |       ^
      """.stripMargin
  }

  case object MultipleWorkflows extends ErrorWdl {
    val testString = "detect when multiple workflows are defined"
    val wdl =
      """import "ps"
        |import "cgrep"
        |
        |workflow three_step {
        |  call ps.ps
        |  call cgrep.cgrep {
        |    input: in_file=ps.procs
        |  }
        |}
        |workflow BAD {}
      """.stripMargin

    val errors =
      """ERROR: Only one workflow definition allowed, found 2 workflows:
        |
        |Prior workflow definition (line 4 col 10):
        |
        |workflow three_step {
        |         ^
        |
        |Prior workflow definition (line 10 col 10):
        |
        |workflow BAD {}
        |         ^
      """.stripMargin
  }

  case object WorkflowAndNamespaceNameCollision extends ErrorWdl {
    val testString = "detect when a namespace and a workflow have the same name"
    val wdl =
      """import "ps" as ps
        |workflow ps {
        |  call ps.ps
        |}
      """.stripMargin

    val errors =
      """ERROR: Sibling nodes have conflicting names:
        |
        |Namespace defined here (line 1, col 16):
        |
        |import "ps" as ps
        |               ^
        |
        |Workflow statement defined here (line 2, col 10):
        |
        |workflow ps {
        |         ^
      """.stripMargin
  }

  case object NamespaceNameCollision extends ErrorWdl {
    val testString = "detect when two namespaces have the same name"
    val wdl =
      """import "ps" as ps
        |import "cgrep" as ps
        |
        |workflow test {}
      """.stripMargin

    val errors =
      """ERROR: Sibling nodes have conflicting names:
        |
        |Namespace defined here (line 1, col 16):
        |
        |import "ps" as ps
        |               ^
        |
        |Namespace statement defined here (line 2, col 19):
        |
        |import "cgrep" as ps
        |                  ^
      """.stripMargin
  }

  case object BadMemberAccessInCallInputSection extends ErrorWdl {
    val testString = "detect when the RHS of a member access is invalid"
    val wdl =
      """import "ps"
        |import "cgrep"
        |workflow three_step {
        |  call ps.ps
        |  call cgrep.cgrep {
        |    input: pattern=ps.BAD
        |  }
        |}
      """.stripMargin

    val errors =
      """ERROR: Expression references input on call that doesn't exist (line 6, col 23):
        |
        |    input: pattern=ps.BAD
        |                      ^
      """.stripMargin
  }

  case object BadMemberAccessInCallInputSection2 extends ErrorWdl {
    val testString = "detect when the LHS of a member access is invalid"
    val wdl =
      """import "ps"
        |import "cgrep"
        |workflow three_step {
        |  call ps.ps
        |  call cgrep.cgrep {
        |    input: pattern=psBAD.procs
        |  }
        |}
      """.stripMargin

    val errors =
      """ERROR: Expression will not evaluate (line 6, col 26):
        |
        |    input: pattern=psBAD.procs
        |                         ^
      """.stripMargin
  }

  case object UnexpectedEof extends ErrorWdl {
    val testString = "detect when there was an unexpected EOF"
    val wdl = "workflow"

    val errors =
      """ERROR: No more tokens.  Expecting identifier
        |
        |workflow
        |^
      """.stripMargin
  }

  case object UnexpectedSymbol extends ErrorWdl {
    val testString = "detect an unexpected symbol"
    val wdl = "workflow foo workflow"

    val errors =
      """ERROR: Unexpected symbol (line 1, col 14) when parsing 'workflow'.
        |
        |Expected lbrace, got workflow.
        |
        |workflow foo workflow
        |             ^
        |
        |$workflow = :workflow :identifier :lbrace $_gen11 :rbrace -> Workflow( name=$1, body=$3 )
      """.stripMargin
  }

  case object ExtraneousSymbol extends ErrorWdl {
    val testString = "detect when there are extraneous symbols in the source code"
    val wdl = "workflow foo {}}"

    val errors =
      """ERROR: Finished parsing without consuming all tokens.
        |
        |workflow foo {}}
        |               ^
      """.stripMargin
  }

  case object MultipleCallStatementsHaveTheSameName extends ErrorWdl {
    val testString = "detect when a workflow has two calls with the same name"
    val wdl =
      """task x {
        |  command { ps }
        |}
        |
        |workflow wf {
        |  call x
        |  call x
        |}
      """.stripMargin

    val errors =
      """ERROR: Two or more calls have the same name:
        |
        |Call statement here (line 6, column 8):
        |
        |  call x
        |       ^
        |
        |Call statement here (line 7, column 8):
        |
        |  call x
        |       ^
        |
      """.stripMargin
  }

  case object MultipleCallInputSections extends ErrorWdl {
    val testString = "detect when a call has multiple input sections"
    val wdl =
      """task x {
        |  String a
        |  String b
        |  command {  ./script ${a} ${b} }
        |}
        |
        |workflow wf {
        |  call x {
        |    input: a = "a"
        |    input: b = "b"
        |  }
        |}
      """.stripMargin

    val errors =
      """ERROR: Call has multiple 'input' sections defined:
        |
        |    input: b = "b"
        |           ^
        |
        |Instead of multiple 'input' sections, use commas to separate the values.
      """.stripMargin
  }

  case object MapParameterizedTypes extends ErrorWdl {
    val testString = "detect when missing a type parameter on Map instantiation"
    val wdl =
      """workflow w {
        |  Map[Int] i
        |}
      """.stripMargin

    val errors =
      """ERROR: Map type should have two parameterized types (line 2, col 3):
        |
        |  Map[Int] i
        |  ^
      """.stripMargin
  }

  case object TypeMismatch1 extends ErrorWdl {
    val testString = "detect when a call output has a type mismatch (1)"
    val wdl =
      """task a {
        |  command { ./script }
        |  output {
        |    Array[String] x = "bad value"
        |  }
        |}
        |
        |workflow w {
        |  call a
        |}
      """.stripMargin

    val errors =
      """ERROR: x is declared as a Array[String] but the expression evaluates to a String:
        |
        |    Array[String] x = "bad value"
        |                  ^
      """.stripMargin
  }

  case object TypeMismatch2 extends ErrorWdl {
    val testString = "detect when a call output has a type mismatch (2)"
    val wdl =
      """task a {
        |  command { ./script }
        |  output {
        |    Int x = "bad value"
        |  }
        |}
        |
        |workflow w {
        |  call a
        |}
      """.stripMargin

    val errors =
      """ERROR: Value for x is not coerceable into a Int:
        |
        |    Int x = "bad value"
        |        ^
      """.stripMargin
  }

  case object TypeMismatch3 extends ErrorWdl {
    val testString = "detect when a call output has a type mismatch (3)"
    val wdl =
      """task a {
        |  command { .. }
        |  output {
        |    Int x = 2
        |  }
        |}
        |
        |workflow w {
        |  Array[Int] arr = [1]
        |  call a
        |  
        |  output {
        |    Boolean o = a.x
        |  }
        |}
      """.stripMargin

    val errors =
      """ERROR: o is declared as a Boolean but the expression evaluates to a Int:
        |
        |    Boolean o = a.x
        |            ^""".stripMargin
  }
  
  case object TypeMismatch4 extends ErrorWdl {
    val testString = "detect when a call output has a type mismatch (4)"
    val wdl =
      """task a {
        |  command { .. }
        |  output {
        |    Int x = 2
        |  }
        |}
        |
        |workflow w {
        |  Array[Int] arr = [1]
        |  scatter(i in arr) {
        |    call a
        |  }
        |  
        |  output {
        |    Int o = a.x
        |  }
        |}
      """.stripMargin

    val errors =
      """ERROR: o is declared as a Int but the expression evaluates to a Array[Int]:
        |
        |    Int o = a.x
        |        ^""".stripMargin
  }

  case object MetaSectionStringValues extends ErrorWdl {
    val testString = "detect when meta section contains a non-string value"
    val wdl =
      """task a {
        |  command { ./script }
        |  meta {
        |    foo: 1+1
        |  }
        |}
        |
        |workflow w {
        |  call a
        |}
      """.stripMargin

    val errors =
      """ERROR: Value for this attribute is expected to be a string:
        |
        |    foo: 1+1
        |    ^
      """.stripMargin
  }

  case object MultipleCommandSections extends ErrorWdl {
    val testString = "detect when a task specifies two command sections"
    val wdl =
      """task a {
        |  command { ./script }
        |  command { ps }
        |}
        |
        |workflow w {
        |  call a
        |}
      """.stripMargin

    val errors =
      """ERROR: Expecting to find at most one 'command' section in the task:
        |
        |task a {
        |     ^
      """.stripMargin
  }

  case object CommandExpressionVariableReferenceIntegrity extends ErrorWdl {
    val testString = s"detect when expressions in command section reference only inputs of the task"
    val wdl =
      """task a {
        |  Int x
        |  command { ./script ${x+y} }
        |}
        |
        |workflow w {
        |  call a
        |}
      """.stripMargin

    val errors =
      """ERROR: Variable y does not reference any declaration in the task (line 3, col 26):
        |
        |  command { ./script ${x+y} }
        |                         ^
        |
        |Task defined here (line 1, col 6):
        |
        |task a {
        |     ^
      """.stripMargin
  }

  case object DeclarationVariableReferenceIntegrity extends ErrorWdl {
    val testString = "detect when a task declaration references a variable that wasn't declared (1)"
    val wdl =
      """task a {
        |  Int x
        |  Int y = x + z
        |  command { ./script ${x} }
        |}
        |
        |workflow w {
        |  call a {input: x=5}
        |}
      """.stripMargin

    val errors =
      """ERROR: Variable z does not reference any declaration in the task (line 3, col 15):
        |
        |  Int y = x + z
        |              ^
        |
        |Declaration starts here (line 3, col 7):
        |
        |  Int y = x + z
        |      ^
      """.stripMargin
  }

  case object DeclarationVariableReferenceIntegrity2 extends ErrorWdl {
    val testString = "detect when a task declaration references a variable that wasn't declared (2)"
    val wdl =
      """task a {
        |  Int x
        |  Int y = x + z
        |  Int z
        |  command { ./script ${x} }
        |}
        |
        |workflow w {
        |  call a {input: x=5}
        |}
      """.stripMargin

    val errors =
      """ERROR: Variable z does not reference any declaration in the task (line 3, col 15):
        |
        |  Int y = x + z
        |              ^
        |
        |Declaration starts here (line 3, col 7):
        |
        |  Int y = x + z
        |      ^
      """.stripMargin
  }

  case object MultipleVariableDeclarationsInScope extends ErrorWdl {
    val testString = "detect when a variable is declared more than once (1)"
    val wdl =
      """task inputOops {
        |  Int a = 5
        |  Int a = 10
        |  command { echo ${a} }
        |}
        |workflow a { call inputOops }
      """.stripMargin

    val errors =
      """ERROR: Sibling nodes have conflicting names:
        |
        |Declaration defined here (line 2, col 3):
        |
        |  Int a = 5
        |  ^
        |
        |Declaration statement defined here (line 3, col 3):
        |
        |  Int a = 10
        |  ^
      """.stripMargin
  }

  case object MultipleVariableDeclarationsInScope2 extends ErrorWdl {
    val testString = "detect when a variable is declared more than once (2)"
    val wdl =
      """task outputOops {
        |  command { echo 5 }
        |  output {
        |    Int b = 5
        |    Int b = 10
        |  }
        |}
        |workflow b { call outputOops }
      """.stripMargin

    val errors =
      """ERROR: Sibling nodes have conflicting names:
        |
        |CallOutput defined here (line 4, col 5):
        |
        |    Int b = 5
        |    ^
        |
        |CallOutput statement defined here (line 5, col 5):
        |
        |    Int b = 10
        |    ^
      """.stripMargin
  }

  case object MultipleVariableDeclarationsInScope3 extends ErrorWdl {
    val testString = "detect when a variable is declared more than once (3)"
    val wdl =
      """workflow c { 
        | output {
        |   String o = "output"
        |   String o = "output2"
        | }
        |}
      """.stripMargin

    val errors =
      """ERROR: Sibling nodes have conflicting names:
        |
        |WorkflowOutput defined here (line 3, col 4):
        |
        |   String o = "output"
        |   ^
        |
        |WorkflowOutput statement defined here (line 4, col 4):
        |
        |   String o = "output2"
        |   ^
      """.stripMargin
  }

  case object MultipleVariableDeclarationsInScope4 extends ErrorWdl {
    val testString = "detect when a variable is declared more than once (4)"
    val wdl =
      """task inputOutputOops {
        |  Int c = 5
        |  command { echo 5 }
        |  output {
        |    Int c = 10
        |  }
        |}
        |workflow c { call inputOutputOops }
      """.stripMargin

    val errors =
      """ERROR: Sibling nodes have conflicting names:
        |
        |Declaration defined here (line 2, col 3):
        |
        |  Int c = 5
        |  ^
        |
        |CallOutput statement defined here (line 5, col 5):
        |
        |    Int c = 10
        |    ^
      """.stripMargin
  }

  case object MultipleVariableDeclarationsInScope5 extends ErrorWdl {
    val testString = "detect when a variable is declared more than once (5)"
    val wdl =
      """task t {
        | command {...}
        | output {
        |   String o = "output"
        | }
        |}
        |
        |workflow c {
        | call t
        | 
        | output {
        |   t.*
        |   t.o
        | }
        |}
      """.stripMargin

    val errors =
      """|ERROR: Sibling nodes have conflicting names:
        |
        |WorkflowOutput defined here (line 4, col 4):
        |
        |   String o = "output"
        |   ^
        |
        |WorkflowOutput statement defined here (line 4, col 4):
        |
        |   String o = "output"
        |   ^
      """.stripMargin
  }

  case object WorkflowOutputReferenceNonExistingCall extends ErrorWdl {
    val testString = "detect when a workflow output references a non existing call"
    val wdl =
      """task t {
        | command {...}
        | output {
        |   String o = "output"
        | }
        |}
        |
        |workflow c {
        | output {
        |   t.o
        | }
        |}
      """.stripMargin

    val errors =
      """ERROR: Expression references output on call that doesn't exist (line 10, col 4):
        |
        |   t.o
        |   ^
      """.stripMargin
  }

  val syntaxErrorWdlTable = Table(
    "errorWdl",
    CallReferencesBadInput,
    CallReferencesBadTask,
    MultipleWorkflows,
    WorkflowAndNamespaceNameCollision,
    NamespaceNameCollision,
    BadMemberAccessInCallInputSection,
    BadMemberAccessInCallInputSection2,
    UnexpectedEof,
    UnexpectedSymbol,
    ExtraneousSymbol,
    MultipleCallStatementsHaveTheSameName,
    MultipleCallInputSections,
    MapParameterizedTypes,
    TypeMismatch1,
    TypeMismatch2,
    TypeMismatch3,
    TypeMismatch4,
    MetaSectionStringValues,
    MultipleCommandSections,
    CommandExpressionVariableReferenceIntegrity,
    DeclarationVariableReferenceIntegrity,
    DeclarationVariableReferenceIntegrity2,
    MultipleVariableDeclarationsInScope,
    MultipleVariableDeclarationsInScope2,
    MultipleVariableDeclarationsInScope3,
    MultipleVariableDeclarationsInScope4,
    MultipleVariableDeclarationsInScope5,
    WorkflowOutputReferenceNonExistingCall
  )

  forAll(syntaxErrorWdlTable) { (errorWdl) =>
    it should errorWdl.testString in {
      try {
        WdlNamespace.loadUsingSource(errorWdl.wdl, None, Option(Seq(resolver)))
        fail("Expecting a SyntaxError")
      } catch {
        case e: SyntaxError =>
          normalizeErrorMessage(e.getMessage) shouldEqual normalizeErrorMessage(errorWdl.errors)
      }
    }
  }
}

