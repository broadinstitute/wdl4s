package cwl

import org.scalatest.{FlatSpec, Matchers}
import wdl.values.WdlString
import wom.expression.PlaceholderIoFunctionSet

class CwlExpressionCommandPartSpec extends FlatSpec with Matchers {

  behavior of "CwlExpressionCommandPart"

  it should "instantiate" in {
    // NOTE: toFixed used to remove the fraction part of ECMAScript numbers
    // https://stackoverflow.com/questions/25989642/why-does-java-8-nashorn-javascript-modulo-returns-0-0-double-instead-of-0-i#answer-25991982
    // https://community.apigee.com/questions/33936/javascript-parseint-not-converting-to-int-value-ne.html
    val commandPart = CwlExpressionCommandPart("$(parseInt(inputs.myStringInt).toFixed())")
    val result = commandPart.instantiate(Map("myStringInt" -> WdlString("3")), PlaceholderIoFunctionSet, identity)
    result should be("3")
  }

}