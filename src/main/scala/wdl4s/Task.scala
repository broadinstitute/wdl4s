package wdl4s

import wdl4s.AstTools.{AstNodeName, EnhancedAstNode}
import wdl4s.WdlExpression.ScopedLookupFunction
import wdl4s.command.{CommandPart, ParameterCommandPart, StringCommandPart}
import wdl4s.expression.{NoFunctions, WdlFunctions, WdlStandardLibraryFunctionsType}
import wdl4s.parser.WdlParser._
import wdl4s.types.WdlType
import wdl4s.util.StringUtil
import wdl4s.values.WdlValue

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object Task {

  def apply(ast: Ast, wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): Task = {
    val taskNameTerminal = ast.getAttribute("name").asInstanceOf[Terminal]
    val name = taskNameTerminal.sourceString
    val commandAsts = ast.findAsts(AstNodeName.Command)
    val runtimeAttributes = RuntimeAttributes(ast)
    val meta = wdlSectionToStringMap(ast, AstNodeName.Meta, wdlSyntaxErrorFormatter)
    val parameterMeta = wdlSectionToStringMap(ast, AstNodeName.ParameterMeta, wdlSyntaxErrorFormatter)
    val outputs = ast.findAsts(AstNodeName.Output) map { TaskOutput(_, wdlSyntaxErrorFormatter) }

    if (commandAsts.size != 1) throw new SyntaxError(wdlSyntaxErrorFormatter.expectedExactlyOneCommandSectionPerTask(taskNameTerminal))
    val commandTemplate = commandAsts.head.getAttribute("parts").asInstanceOf[AstList].asScala.toVector map {
      case x: Terminal => new StringCommandPart(x.getSourceString)
      case x: Ast => ParameterCommandPart(x, wdlSyntaxErrorFormatter)
    }

    Task(name, commandTemplate, runtimeAttributes, meta, parameterMeta, outputs, ast)
  }

  private def wdlSectionToStringMap(taskAst: Ast, node: String, wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter): Map[String, String] = {
    taskAst.findAsts(node) match {
      case a if a.isEmpty => Map.empty[String, String]
      case a if a.size == 1 =>
        // Yes, even 'meta {}' and 'parameter_meta {}' sections have RuntimeAttribute ASTs.
        // In hindsight, this was a poor name for the AST.
        a.head.findAsts(AstNodeName.RuntimeAttribute).map({ ast =>
          val key = ast.getAttribute("key").asInstanceOf[Terminal]
          val value = ast.getAttribute("value")
          if (!value.isInstanceOf[Terminal] || value.asInstanceOf[Terminal].getTerminalStr != "string") {
            // Keys are parsed as identifiers, but values are parsed as expressions.
            // For now, only accept expressions that are strings
            throw new SyntaxError(wdlSyntaxErrorFormatter.expressionExpectedToBeString(key))
          }
          key.sourceString -> value.sourceString
        }).toMap
      case _ => throw new SyntaxError(wdlSyntaxErrorFormatter.expectedAtMostOneSectionPerTask(node, taskAst.getAttribute("name").asInstanceOf[Terminal]))
    }
  }

  def empty: Task = new Task("taskName", Seq.empty, RuntimeAttributes(Map.empty[String, WdlExpression]), Map.empty, Map.empty, Seq.empty, null)
}

/**
 * Represents a `task` declaration in a WDL file
 *
 * @param name Name of the task
 * @param commandTemplate Sequence of command pieces, essentially a parsed command template
 * @param runtimeAttributes 'runtime' section of a file
 * @param meta 'meta' section of a task
 * @param parameterMeta - 'parameter_meta' section of a task
 * @param outputs Set of defined outputs in the `output` section of the task
 * @param ast The syntax tree from which this was built.
 */
case class Task(name: String,
                commandTemplate: Seq[CommandPart],
                runtimeAttributes: RuntimeAttributes,
                meta: Map[String, String],
                parameterMeta: Map[String, String],
                outputs: Seq[TaskOutput],
                ast: Ast) extends Scope {

  override val unqualifiedName: LocallyQualifiedName = name

  /**
    * Given a map of task-local parameter names and WdlValues, create a command String.
    *
    * Instantiating a command line is the process of taking a command in this form:
    *
    * {{{
    *   sh script.sh ${var1} -o ${var2}
    * }}}
    *
    * This command is stored as a `Seq[CommandPart]` in the `Command` class (e.g. [sh script.sh, ${var1}, -o, ${var2}]).
    * Then, given a map of variable -> value:
    *
    * {{{
    * {
    *   "var1": "foo",
    *   "var2": "bar"
    * }
    * }}}
    *
    * It calls instantiate() on each part, and passes this map. The ParameterCommandPart are the ${var1} and ${var2}
    * pieces and they lookup var1 and var2 in that map.
    *
    * The command that's returned from Command.instantiate() is:
    *
    * {{{sh script.sh foo -o bar}}}
    *
    * @param parameters Map[String, WdlValue] of inputs to this call, keys should be declarations
    * @param functions Implementation of the WDL standard library functions to evaluate functions in expressions
    * @param valueMapper Optional WdlValue => WdlValue function that is called on the result of each expression
    *                    evaluation (i.e. evaluation of ${...} blocks).
    * @return String instantiation of the command
    */
  def instantiateCommand(parameters: CallInputs,
                         functions: WdlFunctions[WdlValue],
                         valueMapper: WdlValue => WdlValue = (v) => v): Try[String] = {
    Try(StringUtil.normalize(commandTemplate.map(_.instantiate(this, parameters, functions, valueMapper)).mkString("")))
  }

  def commandTemplateString: String = StringUtil.normalize(commandTemplate.map(_.toString).mkString)

  override def toString: String = s"[Task name=$name commandTemplate=$commandTemplate}]"
}
