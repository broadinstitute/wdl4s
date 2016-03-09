package wdl4s

import java.io.File

import wdl4s.AstTools.{AstNodeName, EnhancedAstNode, EnhancedAstSeq}
import wdl4s.expression.{WdlFunctions, WdlStandardLibraryFunctions}
import wdl4s.parser.WdlParser
import wdl4s.parser.WdlParser._
import wdl4s.types._
import wdl4s.util.FileUtil.EnhancedFile
import wdl4s.util.TryUtil
import wdl4s.values._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
  * Represents a parsed WDL file
  */
sealed trait WdlNamespace extends WdlValue with Scope {
  final val wdlType = WdlNamespaceType
  def ast: Ast
  def importedAs: Option[String] // Used when imported with `as`
  def imports: Seq[Import]
  def namespaces: Seq[WdlNamespace]
  def tasks: Seq[Task]
  def workflows: Seq[Workflow]
  def terminalMap: Map[Terminal, WdlSource]
  def findTask(name: String): Option[Task] = tasks.find(_.name == name)
  override def unqualifiedName: LocallyQualifiedName = importedAs.getOrElse("")
  override def appearsInFqn: Boolean = importedAs.isDefined
  def resolve(fqn: FullyQualifiedName): Option[Scope] = {
    descendants.find(d => d.fullyQualifiedName == fqn || d.fullyQualifiedNameWithIndexScopes == fqn)
  }
}

/**
 * A WdlNamespace which doesn't have a locally defined Workflow.
 */
case class WdlNamespaceWithoutWorkflow(importedAs: Option[String],
                                       imports: Seq[Import],
                                       namespaces: Seq[WdlNamespace],
                                       tasks: Seq[Task],
                                       terminalMap: Map[Terminal, WdlSource],
                                       ast: Ast) extends WdlNamespace {
  val workflows = Seq.empty[Workflow]
}

/**
  * A WdlNamespace which has exactly one workflow defined.
  */
case class WdlNamespaceWithWorkflow(importedAs: Option[String],
                                    workflow: Workflow,
                                    imports: Seq[Import],
                                    namespaces: Seq[WdlNamespace],
                                    tasks: Seq[Task],
                                    terminalMap: Map[Terminal, WdlSource],
                                    wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter,
                                    ast: Ast) extends WdlNamespace {
  val workflows = Seq(workflow)
  override def toString: String = s"[WdlNamespace importedAs=$importedAs]"

  /**
    * Confirm all required inputs are present and attempt to coerce raw inputs to `WdlValue`s.
    * This can fail if required raw inputs are missing or if the values for a specified raw input
    * cannot be coerced to the target type of the input as specified in the namespace.
    */
  def coerceRawInputs(rawInputs: WorkflowRawInputs): Try[WorkflowCoercedInputs] = {
    def coerceRawInput(input: WorkflowInput): Try[Option[WdlValue]] = input.fqn match {
      case _ if rawInputs.contains(input.fqn) =>
        val rawValue = rawInputs.get(input.fqn).get
        input.wdlType.coerceRawValue(rawValue) match {
          case Success(value) => Success(Some(value))
          case _ => Failure(new UnsatisfiedInputsException(s"Could not coerce value for '${input.fqn}' into: ${input.wdlType}"))
        }
      case _ =>
        input.optional match {
          case true => Success(None)
          case _ => Failure(new UnsatisfiedInputsException(s"Required workflow input '${input.fqn}' not specified."))
        }
    }

    val tryCoercedValues = workflow.inputs map { case (fqn, input) => fqn -> coerceRawInput(input) }

    val (successes, failures) = tryCoercedValues.partition { case (_, tryValue) => tryValue.isSuccess }
    if (failures.isEmpty) {
      Try(for {
        (key, tryValue) <- successes
        optionValue = tryValue.get if tryValue.get.isDefined
      } yield key -> optionValue.get)
    } else {
      val errors = failures.values.collect { case f: Failure[_] => f.exception.getMessage }
      // .get because failures is guaranteed to be nonEmpty
      import scalaz.Scalaz._
      Failure(new ValidationException("Workflow input processing failed.", errors.toList.toNel.get))
    }
  }

  /**
    * Some declarations need a value from the user and some have an expression attached to them.
    * For the declarations that have an expression attached to it already, evaluate the expression
    * and return the value for storage in the symbol store
    */
  def staticDeclarationsRecursive(userInputs: WorkflowCoercedInputs, wdlFunctions: WdlStandardLibraryFunctions): Try[WorkflowCoercedInputs] = {
    def evalDeclaration(accumulated: Map[FullyQualifiedName, Try[WdlValue]], current: NewDeclaration): Map[FullyQualifiedName, Try[WdlValue]] = {
      current.expression match {
        case Some(expr) =>
          val successfulAccumulated = accumulated.collect({ case (k, v) if v.isSuccess => k -> v.get })
          val value = expr.evaluate(current.lookupFunction(successfulAccumulated ++ userInputs, wdlFunctions, Map.empty[Scatter, Int]), wdlFunctions)
          accumulated + (current.fullyQualifiedName -> value)
        case None => accumulated
      }
    }

    // TODO: sfrazer: add namespace level declarations here
    def evalScope(scope: Scope): Map[FullyQualifiedName, Try[WdlValue]] = {
      val evaledDeclarations = scope.declarations.foldLeft(Map.empty[FullyQualifiedName, Try[WdlValue]])(evalDeclaration)
      val nonTasks = scope.children.collect({
        case n: GraphNode => n
        case w: Workflow => w
      })
      evaledDeclarations ++ nonTasks.flatMap(evalScope).toMap
    }

    TryUtil.sequenceMap(evalScope(this))
  }
}

/**
 * Main interface into the `wdl4s` package.
 *
 * Example usage
 *
 * {{{
 * val namespace = WdlNamespace.load(new File("/path/to/file.wdl"))
 * namespace.workflow.calls foreach { call =>
 *   println(call)
 * }
 * }}}
 */
object WdlNamespace {

  def load(wdlFile: File): WdlNamespace = {
    load(readFile(wdlFile), wdlFile.toString, localImportResolver, None)
  }

  def load(wdlFile: File, importResolver: ImportResolver): WdlNamespace = {
    load(readFile(wdlFile), wdlFile.toString, importResolver, None)
  }

  def load(wdlSource: WdlSource): WdlNamespace = {
    load(wdlSource, "string", localImportResolver, None)
  }

  def load(wdlSource: WdlSource, importResolver: ImportResolver): WdlNamespace = {
    load(wdlSource, "string", importResolver, None)
  }

  def load(wdlSource: WdlSource, resource: String): WdlNamespace = {
    load(wdlSource, resource, localImportResolver, None)
  }

  def load(wdlSource: WdlSource, resource: String, importResolver: ImportResolver): WdlNamespace = {
    load(wdlSource, resource, importResolver, None)
  }

  private def load(wdlSource: WdlSource, resource: String, importResolver: ImportResolver, importedAs: Option[String]): WdlNamespace = {
    WdlNamespace(AstTools.getAst(wdlSource, resource), wdlSource, importResolver, importedAs, root=true)
  }

  private def loadChild(wdlSource: WdlSource, resource: String, importResolver: ImportResolver, importedAs: Option[String]): WdlNamespace = {
    WdlNamespace(AstTools.getAst(wdlSource, resource), wdlSource, importResolver, importedAs, root=false)
  }

  def apply(ast: Ast, source: WdlSource, importResolver: ImportResolver, namespaceName: Option[String], root: Boolean = false): WdlNamespace = {
    val imports = ast.getAttribute("imports").astListAsVector.map(Import(_))

    val namespaces: Seq[WdlNamespace] = for {
      i <- imports
      source = importResolver(i.uri)
      if source.length > 0
    } yield WdlNamespace.loadChild(source, i.uri, importResolver, i.namespaceName)

    /** namespacesWithNames (i.e. import "foo.wdl" as ns) are kept around as sub-namespaces
      * namespacesWithoutNames (i.e. import "foo.wdl") are treated as c-style #includes where the tasks imported into this namespace
      */
    val (namespacesWithNames, namespacesWithoutNames) = namespaces.partition(_.importedAs.isDefined)

    /**
      * Map of Terminal -> WDL Source Code so the syntax error formatter can show line numbers
      */
    val terminalMap = AstTools.terminalMap(ast, source)
    val combinedTerminalMap = (namespaces.map(_.terminalMap) ++ Seq(terminalMap)) reduce (_ ++ _)
    val wdlSyntaxErrorFormatter = new WdlSyntaxErrorFormatter(combinedTerminalMap)

    /**
     * All imported `task` definitions for `import` statements without a namespace (i.e. no `as` clause)
     * These tasks are considered to be in this current namespace
     */
    val importedTasks: Seq[Task] = namespacesWithoutNames.flatMap(_.tasks)

    /**
     * All `task` definitions defined in the WDL file (i.e. not imported)
     */
    val localTasks: Seq[Task] = ast.findAsts(AstNodeName.Task).map(Task(_, wdlSyntaxErrorFormatter))

    /**
     * All `task` definitions, including local and imported ones
     */
    val tasks: Seq[Task] = localTasks ++ importedTasks

    /**
     * Ensure that no namespace names collide with task names.
     */
    for {
      i <- imports
      namespaceTerminal <- i.namespaceTerminal
      task <- findTask(namespaceTerminal.sourceString, namespacesWithNames, tasks)
    } yield {
      throw new SyntaxError(wdlSyntaxErrorFormatter.taskAndNamespaceHaveSameName(task.ast, namespaceTerminal))
    }

    /**
      * Ensure that no namespace names collide with workflow names
      */
    for {
      workflowAst <- ast.findAsts(AstNodeName.Workflow)
      i <- imports
      namespaceTerminal <- i.namespaceTerminal
      if namespaceTerminal.sourceString == workflowAst.getAttribute("name").sourceString
    } yield {
      throw new SyntaxError(wdlSyntaxErrorFormatter.workflowAndNamespaceHaveSameName(workflowAst, namespaceTerminal))
    }

    /** Detect duplicated task names */
    val dupeTaskAstsByName = tasks.map(_.ast).duplicatesByName
    if (dupeTaskAstsByName.nonEmpty) {
      throw new SyntaxError(wdlSyntaxErrorFormatter.duplicateTask(dupeTaskAstsByName))
    }

    val scopeIndexes: mutable.Map[Class[_ <: Scope], Int] = mutable.HashMap.empty.withDefaultValue(-1)

    def getScope(scopeAst: Ast, scopedTo: Option[Scope]): Scope = {
      val scope = scopeAst.getName match {
        case AstNodeName.Call => Call(scopeAst, namespacesWithNames, tasks, wdlSyntaxErrorFormatter)
        case AstNodeName.Workflow => Workflow(scopeAst, wdlSyntaxErrorFormatter)
        case AstNodeName.Declaration => NewDeclaration(scopeAst, wdlSyntaxErrorFormatter, scopedTo)
        case AstNodeName.Scatter =>
          scopeIndexes(classOf[Scatter]) += 1
          Scatter(scopeAst, scopeIndexes(classOf[Scatter]))
      }

      scope.children = getChildren(scopeAst, Option(scope))
      scope.children.foreach(_.parent = scope)
      scope
    }

    def getChildren(scopeAst: Ast, scope: Option[Scope]): Seq[Scope] = {
      val ScopeAstNames = Seq(
        AstNodeName.Call, AstNodeName.Workflow, AstNodeName.Namespace,
        AstNodeName.Scatter, AstNodeName.If, AstNodeName.Declaration
      )

      def getScopeAsts(root: Ast, astAttribute: String): Seq[Ast] = {
        root.getAttribute(astAttribute).astListAsVector.collect({ case a: Ast if ScopeAstNames.contains(a.getName) => a })
      }

      scopeAst.getName match {
        case AstNodeName.Task => getScopeAsts(scopeAst, "declarations").map(getScope(_, scope))
        case AstNodeName.Declaration => Seq.empty[Scope]
        case AstNodeName.Call =>
          val referencedTask = findTask(scopeAst.getAttribute("task").sourceString, namespacesWithNames, tasks)
          referencedTask match {
            case Some(task) => getScopeAsts(task.ast, "declarations").map(getScope(_, scope))
            // TODO: sfrazer: uh oh... syntax error
            case None => Seq.empty[Scope]
          }
        case AstNodeName.Workflow | AstNodeName.Scatter | AstNodeName.If =>
          getScopeAsts(scopeAst, "body").map(getScope(_, scope))
        case AstNodeName.Namespace =>
          getScopeAsts(scopeAst, "definitions").map(getScope(_, scope))
      }
    }

    // TODO: sfrazer: verify that no a declaration and a call don't have the same name in the same scope!!

    val children = tasks ++ namespacesWithNames ++ getChildren(ast, scope=None)

    val namespace = children.collect({ case w: Workflow => w }) match {
      case Nil => WdlNamespaceWithoutWorkflow(namespaceName, imports, namespacesWithNames, tasks, terminalMap, ast)
      case Seq(workflow) => WdlNamespaceWithWorkflow(ast, workflow, namespaceName, imports, namespacesWithNames, tasks, terminalMap, wdlSyntaxErrorFormatter)
      case _ => throw new SyntaxError(wdlSyntaxErrorFormatter.tooManyWorkflows(ast.findAsts(AstNodeName.Workflow).asJava))
    }

    if (namespaceName.isDefined || root) {
      def descendants(scope: Scope): Seq[Scope] = {
        val children = scope.children
        val childDescendants = scope.children.flatMap({
          case n: WdlNamespace => Seq.empty
          case s => descendants(s)
        })
        children ++ childDescendants
      }

      tasks foreach { task =>
        task.children = getChildren(task.ast, Option(task))
        task.children.foreach(_.parent = task)
      }
      namespace.children = children
      namespace.children.foreach(_.parent = namespace)
      descendants(namespace).foreach(_.namespace = namespace)
    }

    def validateCallInputSection(call: Call): Seq[SyntaxError] = {
      val callInputSections = AstTools.callInputSectionIOMappings(call.ast, wdlSyntaxErrorFormatter)

      val invalidCallInputReferences = callInputSections flatMap { ast =>
        val lhs = ast.getAttribute("key").sourceString
        val rhs = ast.getAttribute("value")
        call.declarations.find(_.unqualifiedName == lhs) match {
          case Some(decl) => None
          case None => Option(new SyntaxError(wdlSyntaxErrorFormatter.callReferencesBadTaskInput(ast, call.task.ast)))
        }
      }

      /**
        * Ensures that the lhs corresponds to a call and the rhs corresponds to one of its outputs. We're only checking
        * top level MemberAccess ASTs because the sub-ASTs don't make sense w/o the context of the parent. For example
        * if we have "input: var=ns.ns1.my_task" it does not make sense to validate "ns1.my_task" by itself as it only
        * makes sense to validate that "ns.ns1.my_task" as a whole is coherent
        *
        * Run for its side effect (i.e. Exception) but we were previously using a Try and immediately calling .get on it
        * so it's the same thing
        */

      val invalidMemberAccesses = callInputSections flatMap { ast =>
        ast.getAttribute("value").findTopLevelMemberAccesses flatMap { memberAccessAst =>
          val memberAccess = MemberAccess(memberAccessAst)
          call.resolveVariable(memberAccess.lhs) match {
            case Some(c: Call) if c.task.outputs.exists(_.name == memberAccess.rhs) => None
            case Some(c: Call) =>
              Option(new SyntaxError(wdlSyntaxErrorFormatter.memberAccessReferencesBadTaskInput(memberAccessAst)))
            case None =>
              Option(new SyntaxError(wdlSyntaxErrorFormatter.undefinedMemberAccess(memberAccessAst)))
          }
        }
      }

      invalidMemberAccesses ++ invalidCallInputReferences
    }

    val callInputSectionErrors = namespace.descendants.collect({ case c: Call => c }).flatMap(validateCallInputSection)
    callInputSectionErrors match {
      case s: Set[SyntaxError] if s.nonEmpty => throw s.head
      case _ =>
    }

    namespace
  }

  /**
   * Given a name, a collection of WdlNamespaces and a collection of Tasks will attempt to find a Task
   * with that name within the WdlNamespaces
   */
  def findTask(name: String, namespaces: Seq[WdlNamespace], tasks: Seq[Task]): Option[Task] = {
    if (name.contains(".")) {
      val parts = name.split("\\.", 2)
      /* This is supposed to resolve a dot-notation'd string (e.g. "a.b.c") by recursively
       * traversing child namespaces or resolving to a task.
       *
       * For example:
       * findTasks("a.b.c") would first find namespace "a" and then return a.findTasks("b.c")
       * a.findTasks("b.c") would call a.b.findTasks("c")
       * a.b.findTasks("c") would return the task named "c" in the "b" namespace
       */
      namespaces.find(_.importedAs.contains(parts(0))) flatMap { x => findTask(parts(1), x.namespaces, x.tasks)}
    } else tasks.find(_.name == name)
  }

  private def localImportResolver(path: String): WdlSource = readFile(new File(path))
  private def readFile(wdlFile: File): WdlSource = wdlFile.slurp
}

object WdlNamespaceWithWorkflow {
  def load(wdlSource: WdlSource): WdlNamespaceWithWorkflow = from(WdlNamespace.load(wdlSource))
  def load(wdlSource: WdlSource, importResolver: ImportResolver): WdlNamespaceWithWorkflow = {
    WdlNamespaceWithWorkflow.from(WdlNamespace.load(wdlSource, importResolver))
  }

  /**
   * Used to safely cast a WdlNamespace to a NamespaceWithWorkflow. Throws an IllegalArgumentException if another
   * form of WdlNamespace is passed to it
   */
  private def from(namespace: WdlNamespace): WdlNamespaceWithWorkflow = {
    namespace match {
      case n: WdlNamespaceWithWorkflow => n
      case _ => throw new IllegalArgumentException("Namespace does not have a local workflow to run")
    }
  }

  def apply(ast: Ast, workflow: Workflow, namespace: Option[String], imports: Seq[Import],
            namespaces: Seq[WdlNamespace], tasks: Seq[Task], terminalMap: Map[Terminal, WdlSource],
            wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): WdlNamespaceWithWorkflow = {
    new WdlNamespaceWithWorkflow(namespace, workflow, imports, namespaces, tasks, terminalMap, wdlSyntaxErrorFormatter, ast)
  }
}
