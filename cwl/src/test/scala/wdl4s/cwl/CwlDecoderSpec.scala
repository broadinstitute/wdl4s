package wdl4s.cwl

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import ammonite.ops._
import shapeless.Coproduct

class CwlDecoderSpec extends Properties("cwl decoder") {
  /*
"" should "" in {

  val cwl = Coproduct[Cwl](Workflow.create(
    name = generate[String]
    ))
  property("make mine a half") {
    forAll { (n: Int) =>
      whenever(n > 1) { n / 2 should be > 0 }
    }
  }

 property("create a workflow that will replace its steps with the actual object it represents") {
   forAll(asciiString, asciiString) {
     (workflowName, commandLineToolName) =>
       //create a file with  this string, assign to a Cwl file run step, and read it back as a result"
       val clt = CommandLineTool.create(commandLineToolName)
       val wfw = Workfow.create(steps = List(WorfklowStep.create(run = clt)))

       encodeCwl(wfw) |> decodeAllCwl

       //Separates out the cwl into nice files that
       encodeAsLinkedFiles(wfw):
       encodeCwl
}
  */

    property("s") = {
      val value = CwlDecoder.decodeAllCwl(pwd/'cwl/'src/'test/'resources/"1st-workflow.cwl").value

      val result = value.unsafeRunSync

      println(result)




      true

    }


}
