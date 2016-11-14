package wdl4s

import better.files.File
import wdl4s.AstTools.EnhancedAstNode
import wdl4s.parser.WdlParser.{Terminal, Ast, AstNode}

object Import {
  def apply(astNode: AstNode): Import = {
    val uri = astNode.asInstanceOf[Ast].getAttribute("uri").sourceString
    val namespaceTerminal = Option(astNode.asInstanceOf[Ast].getAttribute("namespace")).map(_.asInstanceOf[Terminal]).getOrElse {
      astNode.asInstanceOf[Ast].getAttribute("uri").asInstanceOf[Terminal]
    }

    Import(uri, namespaceTerminal)
  }
}

// FIXME: I dislike dragging the AST along but it's necessary for "compile" time error syntax highlighting, argh
case class Import(uri: String, namespaceTerminal: Terminal) {
  val namespaceName: String = File(namespaceTerminal.sourceString).nameWithoutExtension
}
