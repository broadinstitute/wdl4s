package wdl4s

import wdl4s.AstTools.EnhancedAstNode
import wdl4s.expression.WdlFunctions
import wdl4s.parser.WdlParser.{Ast, SyntaxError, Terminal}
import wdl4s.util.StringUtil
import wdl4s.values.WdlValue

import scala.language.postfixOps
import scala.util.Try

object Call {
  def apply(ast: Ast,
            namespaces: Seq[WdlNamespace],
            tasks: Seq[Task],
            wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): Call = {
    val alias: Option[String] = ast.getAttribute("alias") match {
      case x: Terminal => Option(x.getSourceString)
      case _ => None
    }

    val taskName = ast.getAttribute("task").sourceString

    val task = WdlNamespace.findTask(taskName, namespaces, tasks) getOrElse {
      throw new SyntaxError(wdlSyntaxErrorFormatter.callReferencesBadTaskName(ast, taskName))
    }

    val callInputSectionMappings = processCallInput(ast, wdlSyntaxErrorFormatter)

    // TODO: sfrazer: add this syntax error back!
    /*callInputSectionMappings foreach { case (taskParamName, expression) =>
      task.declarations find { decl => decl.unqualifiedName == taskParamName } getOrElse {
        val callInput = AstTools.callInputSectionIOMappings(ast, wdlSyntaxErrorFormatter) find {
          _.getAttribute("key").sourceString == taskParamName
        } getOrElse {
          throw new SyntaxError(s"Can't find call input: $taskParamName")
        }
        throw new SyntaxError(wdlSyntaxErrorFormatter.callReferencesBadTaskInput(callInput, task.ast))
      }
    }*/

    new Call(alias, task, callInputSectionMappings)
  }

  private def processCallInput(ast: Ast,
                               wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): Map[String, WdlExpression] = {
    AstTools.callInputSectionIOMappings(ast, wdlSyntaxErrorFormatter) map { a =>
      val key = a.getAttribute("key").sourceString
      val expression = new WdlExpression(a.getAttribute("value"))
      (key, expression)
    } toMap
  }
}

/**
 * Represents a `call` block in a WDL workflow.  Calls wrap tasks
 * and optionally provide a subset of the inputs for the task (as inputMappings member).
 * All remaining inputs to the task that are not declared in the `input` section
 * of the `call` block are called unsatisfiedInputs
 *
 * @param alias The alias for this call.  If two calls in a workflow use the same task
 *              then one of them needs to be aliased to a different name
 * @param task The task that this `call` will invoke
 * @param inputMappings A map of task-local input names and corresponding expression for the
 *                      value of those inputs
 */
case class Call(alias: Option[String],
                task: Task,
                inputMappings: Map[String, WdlExpression]) extends Scope with GraphNode {
  val unqualifiedName: String = alias getOrElse task.name

  lazy val upstream: Set[Scope with GraphNode] = {
    val dependentNodes = for {
      expr <- inputMappings.values
      variable <- expr.variableReferences
      node <- resolveVariable(variable.sourceString)
    } yield node

    val ancestorScatters = ancestry.collect({ case s: Scatter with GraphNode => s})

    ( dependentNodes ++
      dependentNodes.flatMap(_.upstream) ++
      ancestorScatters ++
      ancestorScatters.flatMap(_.upstream) ).toSet
  }

  lazy val downstream: Set[Scope with GraphNode] = {
    for {
      node <- namespace.descendants.collect({ case n: GraphNode => n }).filter(
        _.fullyQualifiedNameWithIndexScopes != fullyQualifiedNameWithIndexScopes
      )
      if node.upstream.contains(this)
    } yield node
  }

  /**
   * Returns a Seq[WorkflowInput] representing the inputs to the call that are
   * needed before its command can be constructed. This excludes inputs that
   * are satisfied via the 'input' section of the Call definition.
   */
  def unsatisfiedInputs: Seq[WorkflowInput] = for {
    i <- task.declarations if !inputMappings.contains(i.unqualifiedName) && i.expression.isEmpty
  } yield WorkflowInput(s"$fullyQualifiedName.${i.unqualifiedName}", i.wdlType, i.postfixQuantifier)

  override def toString: String = s"[Call name=$unqualifiedName, task=$task]"

  /**
   * Instantiate the abstract command line corresponding to this call using the specified inputs.
    *
   */
  def instantiateCommandLine(inputs: WorkflowCoercedInputs,
                             functions: WdlFunctions[WdlValue],
                             valueMapper: WdlValue => WdlValue = (v) => v): Try[String] = {
    Try(StringUtil.normalize(task.commandTemplate.map(_.instantiate(this, inputs, functions, valueMapper)).mkString("")))
  }

  def workflow: Workflow = parent.map(_.asInstanceOf[Workflow]).getOrElse(throw new IllegalStateException("Grammar constraint violated: Call not in workflow"))
}
