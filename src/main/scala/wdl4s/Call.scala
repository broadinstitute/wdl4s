package wdl4s

import wdl4s.AstTools.EnhancedAstNode
import wdl4s.expression.WdlFunctions
import wdl4s.parser.WdlParser.{Ast, SyntaxError, Terminal}
import wdl4s.util.StringUtil
import wdl4s.values.WdlValue

import scala.language.postfixOps
import scala.util.{Success, Try}

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

    new Call(alias, task, callInputSectionMappings, ast)
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
                inputMappings: Map[String, WdlExpression],
                ast: Ast) extends Scope with GraphNode with WorkflowScoped {
  val unqualifiedName: String = alias getOrElse task.name

  lazy val upstream: Set[Scope with GraphNode] = {
    val dependentNodes = for {
      expr <- inputMappings.values
      variable <- expr.variableReferences
      node <- parent.flatMap(_.resolveVariable(variable.sourceString))
    } yield node

    val firstScatterOrIf = ancestry.collect({
      case s: Scatter with GraphNode => s
      case i: If with GraphNode => i
    }).headOption

    (dependentNodes ++ firstScatterOrIf.toSeq).toSet
  }

  lazy val downstream: Set[Scope with GraphNode] = {
    def expressions(node: GraphNode): Iterable[WdlExpression] = node match {
      case scatter: Scatter => Set(scatter.collection)
      case call: Call => call.inputMappings.values
      case ifStatement: If => Set(ifStatement.condition)
      case declaration: Declaration => declaration.expression.toSet
    }

    for {
      node <- namespace.descendants.collect({ case n: GraphNode => n }).filter(
        _.fullyQualifiedNameWithIndexScopes != fullyQualifiedNameWithIndexScopes
      )
      expression <- expressions(node)
      variable <- expression.variableReferences
      referencedNode = resolveVariable(variable.sourceString)
      if referencedNode == Option(this)
    } yield node
  }

  /**
   * Returns a Seq[WorkflowInput] representing the inputs to the call that are
   * needed before its command can be constructed. This excludes inputs that
   * are satisfied via the 'input' section of the Call definition.
   */
  def unsatisfiedInputs: Seq[WorkflowInput] = for {
    i <- declarations if !inputMappings.contains(i.unqualifiedName) && i.expression.isEmpty
  } yield WorkflowInput(i.fullyQualifiedName, i.wdlType, i.postfixQuantifier)

  override def toString: String = s"[Call $fullyQualifiedName]"

  /**
   * Instantiate the abstract command line corresponding to this call using the specified inputs.
    *
   */
  def instantiateCommandLine(inputs: WorkflowCoercedInputs,
                             functions: WdlFunctions[WdlValue],
                             shards: Map[Scatter, Int] = Map.empty[Scatter, Int],
                             valueMapper: WdlValue => WdlValue = (v) => v): Try[String] = {
    Try(StringUtil.normalize(task.commandTemplate.map(_.instantiate(this, inputs, functions, shards, valueMapper)).mkString("")))
  }

  override def lookupFunction(inputs: WorkflowCoercedInputs,
                              wdlFunctions: WdlFunctions[WdlValue],
                              shards: Map[Scatter, Int] = Map.empty[Scatter, Int],
                              relativeTo: Scope = this): String => WdlValue = {
    def lookup(name: String): WdlValue = {
      val inputMappingsWithMatchingName = Try(
        inputMappings.getOrElse(name, throw new Exception(s"Could not find $name in input section of call $fullyQualifiedName"))
      )

      val declarationsWithMatchingName = Try(
        declarations.find(_.unqualifiedName == name).getOrElse(throw new Exception(s"No declaration named $name for call $fullyQualifiedName"))
      )

      val inputMappingsLookup = for {
        inputExpr <- inputMappingsWithMatchingName
        parent <- Try(parent.getOrElse(throw new Exception(s"Call $unqualifiedName has no parent")))
        evaluatedExpr <- inputExpr.evaluate(parent.lookupFunction(inputs, wdlFunctions, shards, relativeTo), wdlFunctions)
      } yield evaluatedExpr

      val declarationLookup = for {
        declaration <- declarationsWithMatchingName
        inputsLookup <- Try(inputs.getOrElse(declaration.fullyQualifiedName, throw new Exception(s"No input for ${declaration.fullyQualifiedName}")))
      } yield inputsLookup

      val declarationExprLookup = for {
        declaration <- declarationsWithMatchingName
        declarationExpr <- Try(declaration.expression.getOrElse(throw new Exception(s"No expression defined for declaration ${declaration.fullyQualifiedName}")))
        evaluatedExpr <- declarationExpr.evaluate(lookupFunction(inputs, wdlFunctions, shards, relativeTo), wdlFunctions)
      } yield evaluatedExpr

      val taskParentResolution = for {
        parent <- Try(task.parent.getOrElse(throw new Exception(s"Task ${task.unqualifiedName} has no parent")))
        parentLookup <- Try(parent.lookupFunction(inputs, wdlFunctions, shards, relativeTo)(name))
      } yield parentLookup

      val resolutions = Seq(inputMappingsLookup, declarationLookup, declarationExprLookup, taskParentResolution)

      resolutions.collect({ case s: Success[WdlValue] => s}).headOption match {
        case Some(Success(value)) => value
        case None => throw new VariableNotFoundException(name)
      }
    }
    lookup
  }
}
