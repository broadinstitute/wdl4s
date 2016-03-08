package wdl4s

import wdl4s.AstTools.EnhancedAstNode
import wdl4s.types.WdlType
import wdl4s.parser.WdlParser.{Ast, AstNode}

/**
  * Represents a declaration which can show up in a workflow or a task context.  For example
  *
  * task test {
  *   File test_file
  *   command { ... }
  * }
  *
  * workflow wf {
  *   String wf_string = "abc"
  *   call test { input: s=wf_string }
  * }
  *
  * Both the definition of test_file and wf_string are declarations
  */
trait Declaration extends Scope {
  def wdlType: WdlType
  def postfixQuantifier: Option[String]
  def name: String
  def expression: Option[WdlExpression]
  def asTaskInput: Option[TaskInput] = expression match {
    case Some(expr) => None
    case None => Option(TaskInput(name, wdlType, postfixQuantifier))
  }

  def toWdlString: String = {
    val expr = expression.map(e => s" = ${e.toWdlString}").getOrElse("")
    s"${wdlType.toWdlString} $name$expr"
  }
}

object NewDeclaration {
  def apply(ast: Ast, wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter, scopedTo: Option[Scope]): NewDeclaration = {
    NewDeclaration(
      ast.getAttribute("type").wdlType(wdlSyntaxErrorFormatter),
      Option(ast.getAttribute("postfix")).map(_.sourceString),
      ast.getAttribute("name").sourceString,
      ast.getAttribute("expression") match {
        case a: AstNode => Some(WdlExpression(a))
        case _ => None
      },
      scopedTo
    )
  }
}

case class NewDeclaration(wdlType: WdlType,
                          postfixQuantifier: Option[String],
                          unqualifiedName: String,
                          expression: Option[WdlExpression],
                          scopedTo: Option[Scope]) extends Scope with GraphNode with Declaration {
  def name: String = unqualifiedName
  lazy val upstream: Set[Scope with GraphNode] = {
    val dependentNodes = for {
      expr <- expression.toIterable
      variable <- expr.variableReferences
      node <- resolveVariable(variable.sourceString)
    } yield node

    (dependentNodes ++ dependentNodes.flatMap(_.upstream)).toSet
  }

  lazy val downstream: Set[Scope with GraphNode] = {
    for {
      node <- namespace.descendants.collect({ case n: GraphNode => n }).filter(_.fullyQualifiedName != fullyQualifiedName).toSet
      if node.upstream.contains(this)
    } yield node
  }

  override def asTaskInput: Option[TaskInput] = expression match {
    case Some(expr) => None
    case None => Option(TaskInput(unqualifiedName, wdlType, postfixQuantifier))
  }

  def asWorkflowInput: Option[WorkflowInput] = expression match {
    case Some(expr) => None
    case None => Some(WorkflowInput(fullyQualifiedName, wdlType, postfixQuantifier))
  }

  override def toWdlString: String = {
    val expr = expression.map(e => s" = ${e.toWdlString}").getOrElse("")
    s"${wdlType.toWdlString} $unqualifiedName$expr"
  }
}

object Declaration {
  def apply(ast: Ast, wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): Declaration = {
    UnscopedDeclaration(
      ast.getAttribute("type").wdlType(wdlSyntaxErrorFormatter),
      Option(ast.getAttribute("postfix")).map(_.sourceString),
      ast.getAttribute("name").sourceString,
      ast.getAttribute("expression") match {
        case a: AstNode => Some(WdlExpression(a))
        case _ => None
      }
    )
  }
}

case class UnscopedDeclaration(wdlType: WdlType, postfixQuantifier: Option[String], name: String, expression: Option[WdlExpression]) extends Declaration {
  override def unqualifiedName: LocallyQualifiedName = name
}

object ScopedDeclaration {
  def apply(scope: Scope, decl: Declaration): ScopedDeclaration = {
    ScopedDeclaration(scope, decl.wdlType, decl.postfixQuantifier, decl.name, decl.expression)
  }
}

case class ScopedDeclaration(scope: Scope, wdlType: WdlType, postfixQuantifier: Option[String], name: String, expression: Option[WdlExpression]) extends Declaration {
  override def unqualifiedName: LocallyQualifiedName = name
  override def fullyQualifiedName: FullyQualifiedName = s"${scope.fullyQualifiedName}.$name"
  def asWorkflowInput: Option[WorkflowInput] = expression match {
    case Some(expr) => None
    case None => Some(WorkflowInput(fullyQualifiedName, wdlType, postfixQuantifier))
  }
}