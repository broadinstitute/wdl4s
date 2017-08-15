package wdl4s.cwl

import wdl4s.parser.WdlParser.AstNode

/* Placeholders where current CromWOM has no suitable thing, but will have eventually */
object CromwomificationWIPPlaceholders {

  case class AstNodeForLiteralWdlExpression(string:String) extends AstNode {
    override def toPrettyString: String = string

    override def toPrettyString(indent: Int): String = string
  }

}
