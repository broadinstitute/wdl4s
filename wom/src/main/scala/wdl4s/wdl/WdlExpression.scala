package wdl4s.wdl

import wdl4s.parser.WdlParser
import wdl4s.parser.WdlParser.{Ast, AstList, AstNode, Terminal}
import wdl4s.wdl.AstTools.{EnhancedAstNode, VariableReference}
import wdl4s.wdl.WdlCall.outputPortFromNode
import wdl4s.wdl.WdlExpression._
import wdl4s.wdl.expression.{WdlFunctions, _}
import wdl4s.wdl.formatter.{NullSyntaxHighlighter, SyntaxHighlighter}
import wdl4s.wdl.types._
import wdl4s.wdl.values._
import wdl4s.wom.expression.{IoFunctions, VariableLookupContext, WomExpression}
import wdl4s.wom.graph.GraphNodePort
import wdl4s.wom.graph.GraphNodePort.{ConnectedInputPort, GraphNodeOutputPort, InputPort}

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.Try

class WdlExpressionException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

case object NoLookup extends ScopedLookupFunction {
  def apply(value: String): WdlValue =
    throw new UnsupportedOperationException(s"No identifiers should be looked up: $value")
}

class WdlWomExpression(attachedNode: WdlGraphNode, wdlExpression: WdlExpression) extends WomExpression {
  override lazy val name = wdlExpression.name
  
  override def evaluate(variableLookupContext: VariableLookupContext, ioFunctions: IoFunctions): Future[WdlValue] = {
    // TODO figure out functions
    val lf = attachedNode.womLookupFunction(variableLookupContext.inputs, NoFunctions, variableLookupContext.outputResolver, Map.empty, attachedNode)
    Future.fromTry(wdlExpression.evaluate(lf, NoFunctions))
  }
  override lazy val inputPorts: Set[InputPort] = wdlExpression.variablesToOutputPort map {
    case (inputName, outputPort) => ConnectedInputPort(inputName, outputPort.womType, outputPort, _ => this)
  } toSet

  override lazy val outputPorts: Set[GraphNodePort.GraphNodeOutputPort] = {
    // TODO figure something out for name and type here
    Set(GraphNodeOutputPort(wdlExpression + ".output", WdlAnyType, this))
  }
}

object WdlExpression {

  implicit class AstForExpressions(val ast: Ast) extends AnyVal {
    def isFunctionCall: Boolean = ast.getName == "FunctionCall"
    def isBinaryOperator: Boolean = BinaryOperators.contains(ast.getName)
    def isUnaryOperator: Boolean = UnaryOperators.contains(ast.getName)
    def functionName: String = ast.getAttribute("name").asInstanceOf[Terminal].getSourceString
    def isMemberAccess: Boolean = ast.getName == "MemberAccess"
    def isArrayLiteral: Boolean = ast.getName == "ArrayLiteral"
    def isTupleLiteral: Boolean = ast.getName == "TupleLiteral"
    def isMapLiteral: Boolean = ast.getName == "MapLiteral"
    def isObjectLiteral: Boolean = ast.getName == "ObjectLiteral"
    def isArrayOrMapLookup: Boolean = ast.getName == "ArrayOrMapLookup"
    def params: Vector[AstNode] = Option(ast.getAttribute("params")).map(_.asInstanceOf[AstList].asScala.toVector).getOrElse(Vector.empty)
    def name = ast.getAttribute("name").asInstanceOf[Terminal].getSourceString
    def isFunctionCallWithFirstParameterBeingFile = ast.isFunctionCall && ast.params.nonEmpty && WdlFunctionsWithFirstParameterBeingFile.contains(ast.functionName)
    def isGlobFunctionCall = ast.isFunctionCall && ast.params.size == 1 && "glob".equals(ast.functionName)
  }

  implicit class AstNodeForExpressions(val astNode: AstNode) extends AnyVal {
    def containsFunctionCalls: Boolean =
      astNode match {
        case a: Ast if a.isFunctionCall => true
        case a: Ast if a.isBinaryOperator =>
          val lhs = a.getAttribute("lhs")
          val rhs = a.getAttribute("rhs")
          lhs.containsFunctionCalls || rhs.containsFunctionCalls
        case a: Ast if a.isUnaryOperator =>
          val rhs = a.getAttribute("expression")
          rhs.containsFunctionCalls
        case _ => false
      }

    def isTerminal: Boolean = astNode.isInstanceOf[Terminal]
  }

  val parser = new WdlParser()

  /** Maps from a locally qualified name to a WdlValue. */
  type ScopedLookupFunction = String => WdlValue

  val BinaryOperators = Set(
    "Add", "Subtract", "Multiply", "Divide", "Remainder",
    "GreaterThan", "LessThan", "GreaterThanOrEqual", "LessThanOrEqual",
    "Equals", "NotEquals", "LogicalAnd", "LogicalOr"
  )

  val UnaryOperators = Set("LogicalNot", "UnaryPlus", "UnaryNegation")

  val WdlFunctionsWithFirstParameterBeingFile: Seq[String] = Seq(
    "read_int",
    "read_string",
    "read_float",
    "read_boolean",
    "read_lines",
    "read_map",
    "read_object",
    "read_tsv",
    "size"
  )

  def evaluate(ast: AstNode, lookup: ScopedLookupFunction, functions: WdlFunctions[WdlValue]): Try[WdlValue] =
    ValueEvaluator(lookup, functions).evaluate(ast)

  def evaluateFiles(ast: AstNode, lookup: ScopedLookupFunction, functions: WdlFunctions[WdlValue], coerceTo: WdlType = WdlAnyType) =
    FileEvaluator(ValueEvaluator(lookup, functions), coerceTo).evaluate(ast)

  def evaluateType(ast: AstNode, lookup: (String) => WdlType, functions: WdlFunctions[WdlType], from: Option[Scope] = None) =
    TypeEvaluator(lookup, functions, from).evaluate(ast)

  def fromString(expression: WorkflowSource): WdlExpression = {
    val tokens = parser.lex(expression, "string")
    val terminalMap = (tokens.asScala.toVector map {(_, expression)}).toMap
    val parseTree = parser.parse_e(tokens, WdlSyntaxErrorFormatter(terminalMap))
    new WdlExpression(parseTree.toAst, "N/A")
  }

  def toString(ast: AstNode, highlighter: SyntaxHighlighter = NullSyntaxHighlighter): String = {
    ast match {
      case t: Terminal if Seq("identifier", "integer", "float", "boolean").contains(t.getTerminalStr) => t.getSourceString
      case t: Terminal if t.getTerminalStr == "string" => s""""${t.getSourceString.replaceAll("\"", "\\" + "\"")}""""
      case a:Ast if a.isBinaryOperator =>
        val lhs = toString(a.getAttribute("lhs"), highlighter)
        val rhs = toString(a.getAttribute("rhs"), highlighter)
        a.getName match {
          case "Add" => s"$lhs + $rhs"
          case "Subtract" => s"$lhs - $rhs"
          case "Multiply" => s"$lhs * $rhs"
          case "Divide" => s"$lhs / $rhs"
          case "Remainder" => s"$lhs % $rhs"
          case "Equals" => s"$lhs == $rhs"
          case "NotEquals" => s"$lhs != $rhs"
          case "LessThan" => s"$lhs < $rhs"
          case "LessThanOrEqual" => s"$lhs <= $rhs"
          case "GreaterThan" => s"$lhs > $rhs"
          case "GreaterThanOrEqual" => s"$lhs >= $rhs"
          case "LogicalOr" => s"$lhs || $rhs"
          case "LogicalAnd" => s"$lhs && $rhs"
        }
      case a: Ast if a.isUnaryOperator =>
        val expression = toString(a.getAttribute("expression"), highlighter)
        a.getName match {
          case "LogicalNot" => s"!$expression"
          case "UnaryPlus" => s"+$expression"
          case "UnaryNegation" => s"-$expression"
        }
      case TernaryIf(condition, ifTrue, ifFalse) =>
        val c = toString(condition, highlighter)
        val t = toString(ifTrue, highlighter)
        val f = toString(ifFalse, highlighter)
        s"if $c then $t else $f"
      case a: Ast if a.isArrayLiteral =>
        val evaluatedElements = a.getAttribute("values").astListAsVector map {x => toString(x, highlighter)}
        s"[${evaluatedElements.mkString(",")}]"
      case a: Ast if a.isTupleLiteral =>
        val evaluatedElements = a.getAttribute("values").astListAsVector map { x => toString(x, highlighter)}
        s"(${evaluatedElements.mkString(", ")})"
      case a: Ast if a.isMapLiteral =>
        val evaluatedMap = a.getAttribute("map").astListAsVector map { kv =>
          val key = toString(kv.asInstanceOf[Ast].getAttribute("key"), highlighter)
          val value = toString(kv.asInstanceOf[Ast].getAttribute("value"), highlighter)
          s"$key:$value"
        }
        s"{${evaluatedMap.mkString(",")}}"
      case a: Ast if a.isMemberAccess =>
        val lhs = toString(a.getAttribute("lhs"), highlighter)
        val rhs = toString(a.getAttribute("rhs"), highlighter)
        s"$lhs.$rhs"
      case a: Ast if a.isArrayOrMapLookup =>
        val lhs = toString(a.getAttribute("lhs"), highlighter)
        val rhs = toString(a.getAttribute("rhs"), highlighter)
        s"$lhs[$rhs]"
      case a: Ast if a.isFunctionCall =>
        val params = a.params map { a => toString(a, highlighter) }
        s"${highlighter.function(a.name)}(${params.mkString(", ")})"
    }
  }
  
  // TODO: Does the name need to be set later so we can use the FQN of nodes instead of LQNs ?
  def apply(ast: AstNode, name: String): WdlExpression = {
    new WdlExpression(ast, name + ".expression")
  }
}

case class WdlExpression private (ast: AstNode, name: String) extends WdlValue {
  override val wdlType = WdlExpressionType
  
  private var attachedToNode: Option[WdlGraphNode] = None
  
  def attachToNode(wdlGraphNode: WdlGraphNode) = attachedToNode match {
    case None => attachedToNode = Option(wdlGraphNode)
    case Some(_) => throw new IllegalStateException("WdlExpression can only be attached to a node once at construction time.")
  }

  def evaluate(lookup: ScopedLookupFunction, functions: WdlFunctions[WdlValue]): Try[WdlValue] =
    WdlExpression.evaluate(ast, lookup, functions)

  def evaluateFiles(lookup: ScopedLookupFunction, functions: WdlFunctions[WdlValue], coerceTo: WdlType): Try[Seq[WdlFile]] =
    WdlExpression.evaluateFiles(ast, lookup, functions, coerceTo)

  def evaluateType(lookup: (String) => WdlType, functions: WdlFunctions[WdlType], from: Option[Scope] = None): Try[WdlType] =
    WdlExpression.evaluateType(ast, lookup, functions, from)

  def containsFunctionCall = ast.containsFunctionCalls

  def toString(highlighter: SyntaxHighlighter): String = {
    WdlExpression.toString(ast, highlighter)
  }

  override def toWdlString: String = toString(NullSyntaxHighlighter)

  def prerequisiteCallNames: Set[FullyQualifiedName] = {
    this.topLevelMemberAccesses map { _.lhs }
  }
  
  def topLevelMemberAccesses: Set[MemberAccess] = AstTools.findTopLevelMemberAccesses(ast) map { MemberAccess(_) } toSet

  def variableReferences: Iterable[VariableReference] = AstTools.findVariableReferences(ast)

  lazy val variablesToOutputPort: Map[String, GraphNodeOutputPort] = (for {
    variable <- variableReferences
    node <- attachedToNode
    node <- node.resolveVariable(variable.terminal.sourceString)
    outputPort = outputPortFromNode(node, variable.terminalSubIdentifier)
  } yield variable.name -> outputPort).toMap
  
  lazy val toWomExpression: WdlWomExpression = attachedToNode match {
    case Some(node) => new WdlWomExpression(node, this)
    case None => throw new Exception("Cannot convert WdlExpression to WomExpression without a attaching this expression to its WdlGraphNode.")
  }
  
}

object TernaryIf {
  def unapply(arg: Ast): Option[(AstNode, AstNode, AstNode)] = {
    if (arg.getName.equals("TernaryIf")) {
      Option((arg.getAttribute("cond"), arg.getAttribute("iftrue"), arg.getAttribute("iffalse")))
    } else {
      None
    }
  }
}
