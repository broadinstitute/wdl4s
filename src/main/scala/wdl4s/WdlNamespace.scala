package wdl4s

import java.io.File

import wdl4s.AstTools.{AstNodeName, EnhancedAstNode, EnhancedAstSeq}
import wdl4s.expression.WdlStandardLibraryFunctions
import wdl4s.parser.WdlParser
import wdl4s.parser.WdlParser._
import wdl4s.types._
import wdl4s.util.FileUtil.EnhancedFile
import wdl4s.util.TryUtil
import wdl4s.values._

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
 * Define WdlNamespace as a sum type w/ two states - one containing a local workflow and one without.
 * The latter is a valid state for a WDL file, however only the former can be requested to be run, so
 * any constructs (e.g. WorkflowManagerActor) expecting to run a workflow should only take the `NamespaceWithWorkflow`
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

  // Convenience method for findTask in the context of this namespace
  def findTask(name: String): Option[Task] = WdlNamespace.findTask(name, namespaces, tasks)

  override def unqualifiedName: LocallyQualifiedName = "__main__"
  override def appearsInFqn: Boolean = false
  override val parent: Option[Scope] = None
  override def children: Seq[Scope] = tasks ++ workflows
  override val prerequisiteScopes: Set[Scope] = Set.empty[Scope]
  override val prerequisiteCallNames: Set[LocallyQualifiedName] = Set.empty[LocallyQualifiedName]
}

/**
 * A valid Namespace which doesn't have a locally defined Workflow.
 */
case class NamespaceWithoutWorkflow(importedAs: Option[String],
                                    imports: Seq[Import],
                                    namespaces: Seq[WdlNamespace],
                                    tasks: Seq[Task],
                                    terminalMap: Map[Terminal, WdlSource],
                                    ast: Ast) extends WdlNamespace {
  val workflows = Seq.empty[Workflow]
  tasks.foreach(x => x.parent = this)
  workflows.foreach(x => x.parent = this)
}

/** Represents a WdlNamespace which has a local workflow, i.e. a directly runnable namespace */
case class NamespaceWithWorkflow(importedAs: Option[String],
                                 workflow: Workflow,
                                 imports: Seq[Import],
                                 namespaces: Seq[WdlNamespace],
                                 tasks: Seq[Task],
                                 terminalMap: Map[Terminal, WdlSource],
                                 wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter,
                                 ast: Ast) extends WdlNamespace {

  // TODO: sfrazer: this is duplicated
  val workflows = Seq(workflow)
  tasks.foreach(x => x.parent = this)
  workflows.foreach(x => x.parent = this)

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

  private def declarationLookupFunction(decl: ScopedDeclaration,
                                        inputs: Map[FullyQualifiedName, WdlValue],
                                        wdlFunctions: WdlStandardLibraryFunctions)(string: String): WdlValue = {

    /** The `string` parameter might be specified in the 'input' section of a call Declaration in WDL
      *
      * If it does, evaluate the left-hand side expression
      *
      * e.g. call my_task {input: some_var="foo"+"bar"}
      *
      * When evaluating 'some_var' (for example, instantiating the Tasks's command), we want to use the value
      * of "foo"+"bar" as the value
      */
    val inputMappingLookup = decl.scope match {
      case c: Call => c.inputMappings.get(string).flatMap(
        _.evaluate(declarationLookupFunction(decl, inputs, wdlFunctions), wdlFunctions).toOption
      ).toSeq
      case _ => Seq()
    }

    /** This is a scope hierarchy to search for the variable `string`.  If `decl.scopeFqn` == "a.b.c"
      * then `hierarchy` should be Seq("a.b.c", "a.b", "a")
      */
    val hierarchy = decl.scope.fullyQualifiedName.split("\\.").reverse.tails.toSeq.map {_.reverse.mkString(".")}

    /* Attempt to resolve the string in each scope */
    val attemptedValues = inputMappingLookup ++ hierarchy.flatMap { scope => inputs.get(s"$scope.$string") }

    attemptedValues.headOption.getOrElse {
      throw new WdlExpressionException(s"Could not find a value for $string")
    }
  }

  /* Some declarations need a value from the user and some have an expression attached to them.
   * For the declarations that have an expression attached to it already, evaluate the expression
   * and return the value for storage in the symbol store
   */
  def staticDeclarationsRecursive(userInputs: WorkflowCoercedInputs, wdlFunctions: WdlStandardLibraryFunctions): Try[WorkflowCoercedInputs] = {
    def evalDeclaration(accumulated: Map[FullyQualifiedName, Try[WdlValue]], current: ScopedDeclaration): Map[FullyQualifiedName, Try[WdlValue]] = {
      current.expression match {
        case Some(expr) =>
          val successfulAccumulated = accumulated.collect({ case (k, v) if v.isSuccess => k -> v.get })
          val value = expr.evaluate(declarationLookupFunction(current, successfulAccumulated ++ userInputs, wdlFunctions), wdlFunctions)
          accumulated + (current.fullyQualifiedName -> value)
        case None => accumulated
      }
    }

    // declarationsByScope is a Seq[Seq[ScopedDeclaration]] where each Declaration in the Seq[ScopedDeclaration] have the same scope
    val declarationsByScope = workflow.calls.map(_.scopedDeclarations) ++ Seq(workflow.scopedDeclarations)
    val attemptedEvaluations = declarationsByScope.flatMap(d => d.foldLeft(Map.empty[String, Try[WdlValue]])(evalDeclaration)).toMap
    TryUtil.sequenceMap(attemptedEvaluations)
  }

  /**
   * Given a Fully-Qualified Name, return the Scope object that corresponds to this FQN.
   */
  def resolve(fqn: FullyQualifiedName): Option[Scope] =
    (Seq(workflow) ++ workflow.calls ++ workflow.scatters).find(s => s.fullyQualifiedName == fqn || s.fullyQualifiedNameWithIndexScopes == fqn)
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
  /**
   * Given a pointer to a WDL file, parse the text and build Workflow and Task
   * objects.
   *
   * @param wdlFile The file to parse/process
   * @return WdlBinding object with the parsed results
   * @throws WdlParser.SyntaxError if there was a problem parsing the source code
   * @throws UnsupportedOperationException if an error occurred constructing the
   *                                       Workflow and Task objects
   *
   */
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
    WdlNamespace(AstTools.getAst(wdlSource, resource), wdlSource, importResolver, importedAs)
  }

  /**
   * Validates the following things about the AST:
   *
   * 1) Tasks do not have duplicate inputs
   * 2) Tasks in this namespace have unique names
   * 3) Tasks and namespaces don't have overlapping names
   */
  def apply(ast: Ast, source: WdlSource, importResolver: ImportResolver, namespace: Option[String]): WdlNamespace = {
    /**
     * All `import` statement strings at the top of the document
     */
    val imports = ast.getAttribute("imports").asInstanceOf[AstList].asScala map {x => Import(x)}

    /* WdlBinding objects for each import statement */
    val namespaces: Seq[WdlNamespace] = {for {
      i <- imports
      source = importResolver(i.uri) if source.length > 0
    } yield WdlNamespace.load(source, i.uri, importResolver, i.namespace)}.toSeq

    /* Create a map of Terminal -> WdlBinding */
    val terminalMap = AstTools.terminalMap(ast, source)
    val combinedTerminalMap = ((namespaces map {x => x.terminalMap}) ++ Seq(terminalMap)) reduce (_ ++ _)
    val wdlSyntaxErrorFormatter = new WdlSyntaxErrorFormatter(combinedTerminalMap)

    /**
     * All imported `task` definitions for `import` statements without a namespace (e.g. no `as` clause)
     * These tasks are considered to be in this current workspace
     */
    val importedTasks: Seq[Task] = namespaces flatMap { b =>
      b.importedAs match {
        case None => b.tasks
        case _ => Seq.empty[Task]
      }
    }

    /**
     * All `task` definitions defined in the WDL file (i.e. not imported)
     */
    val localTasks: Seq[Task] = ast.findAsts(AstNodeName.Task) map {Task(_, wdlSyntaxErrorFormatter)}

    /**
     * All `task` definitions, including local and imported ones
     */
    val tasks: Seq[Task] = localTasks ++ importedTasks

    /* 
     * Ensure that no namespaces collide with task names. 
     * 
     * It'd be simpler to get this via the `namespaces` themselves but don't have access to the correct AST, which is
     * required by the error syntax highlighter :/ (FIXME: Or do I?)
     */
    for {
      i <- imports
      namespaceAst <- i.namespaceAst
      task <- findTask(namespaceAst.sourceString, namespaces, tasks)
    } yield {throw new SyntaxError(wdlSyntaxErrorFormatter.taskAndNamespaceHaveSameName(task.ast, namespaceAst.asInstanceOf[Terminal]))}

    // Detect duplicated task names
    val dupeTaskAstsByName = tasks.map(_.ast).duplicatesByName
    if (dupeTaskAstsByName.nonEmpty) {
      throw new SyntaxError(wdlSyntaxErrorFormatter.duplicateTask(dupeTaskAstsByName))
    }

    // FIXME: Here's where I'd toSet stuff after duplications are detected
    ast.findAsts(AstNodeName.Workflow) match {
      case Nil => NamespaceWithoutWorkflow(namespace, imports, namespaces, tasks, terminalMap, ast)
      case Seq(x) => NamespaceWithWorkflow(ast, x, namespace, imports, namespaces, tasks, terminalMap, wdlSyntaxErrorFormatter)
      case doh => throw new SyntaxError(wdlSyntaxErrorFormatter.tooManyWorkflows(doh.asJava))
    }
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
      namespaces find {_.importedAs == Some(parts(0))} flatMap {x => findTask(parts(1), x.namespaces, x.tasks)}
    } else tasks.find(_.name == name)
  }

  private def localImportResolver(path: String): WdlSource = readFile(new File(path))
  private def readFile(wdlFile: File): WdlSource = wdlFile.slurp
}

object NamespaceWithWorkflow {
  def load(wdlSource: WdlSource): NamespaceWithWorkflow = from(WdlNamespace.load(wdlSource))
  def load(wdlSource: WdlSource, importResolver: ImportResolver): NamespaceWithWorkflow = {
    NamespaceWithWorkflow.from(WdlNamespace.load(wdlSource, importResolver))
  }
  /**
   * Used to safely cast a WdlNamespace to a NamespaceWithWorkflow. Throws an IllegalArgumentException if another
   * form of WdlNamespace is passed to it
   */
  private def from(namespace: WdlNamespace): NamespaceWithWorkflow = {
    namespace match {
      case n: NamespaceWithWorkflow => n
      case _ => throw new IllegalArgumentException("Namespace does not have a local workflow to run")
    }
  }

  /**
   * Validates:
   * 1) All `Call` blocks reference tasks that exist
   * 2) All `Call` inputs reference actual variables on the corresponding task
   * 3) Calls do not reference the same task input more than once
   * 4) `Call` input expressions (right-hand side) should only use the MemberAccess
   * syntax (e.g: x.y) on WdlObjects (which include other `Call` invocations)
   * 5) `Call` input expressions (right-hand side) should only reference identifiers
   * that will resolve when evaluated
   */
  def apply(ast: Ast, workflowAst: Ast, namespace: Option[String], imports: Seq[Import],
            namespaces: Seq[WdlNamespace], tasks: Seq[Task], terminalMap: Map[Terminal, WdlSource],
            wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): NamespaceWithWorkflow = {
    /*
     * Ensure that no namespaces collide with workflow names.
     *
     * It'd be simpler to get this via the `namespaces` themselves but don't have access to the correct AST, which is
     * required by the error syntax highlighter :/ (FIXME: Or do I?)
     */
    for {
      i <- imports
      namespaceAst <- i.namespaceAst
      if namespaceAst.sourceString == workflowAst.getAttribute("name").sourceString
    } yield {throw new SyntaxError(wdlSyntaxErrorFormatter.workflowAndNamespaceHaveSameName(workflowAst, namespaceAst.asInstanceOf[Terminal]))}

    val workflow: Workflow = Workflow(workflowAst, namespaces, tasks, wdlSyntaxErrorFormatter)

    // Make sure that all MemberAccess ASTs refer to valid calls and those have valid output names
   for {
      call <- workflow.calls
      (name, expression) <- call.inputMappings
      memberAccessAst <- expression.ast.findTopLevelMemberAccesses()
    } validateMemberAccessAst(memberAccessAst, workflow, wdlSyntaxErrorFormatter)

    new NamespaceWithWorkflow(namespace, workflow, imports, namespaces, tasks, terminalMap, wdlSyntaxErrorFormatter, ast)
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
  def validateMemberAccessAst(memberAccessAst: Ast,
                              workflow: Workflow,
                              errorFormatter: WdlSyntaxErrorFormatter): Unit = {
    val memberAccess = MemberAccess(memberAccessAst)
    /*
      This is a shortcut - it's only looking at the workflow's locally qualified name and not the fully qualified
      name. This is ok for now because we do not support nested workflows and all call names must be unique within
      a workflow, however if we support nested workflows this will no longer work properly.
     */
    val call = workflow.calls find { _.unqualifiedName == memberAccess.lhs }

    call match {
      case Some(c) if c.task.outputs.exists(_.name == memberAccess.rhs) => ()
      case Some(c) =>
        throw new SyntaxError(errorFormatter.memberAccessReferencesBadTaskInput(memberAccessAst))
      case None =>
        throw new SyntaxError(errorFormatter.undefinedMemberAccess(memberAccessAst))
    }
  }
}
