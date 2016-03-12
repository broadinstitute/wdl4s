package wdl4s

import wdl4s.expression.WdlFunctions
import wdl4s.parser.WdlParser.Ast
import wdl4s.values.{WdlArray, WdlValue}

import scala.language.postfixOps
import scala.util.{Success, Failure}

trait Scope {
  def unqualifiedName: LocallyQualifiedName
  def appearsInFqn: Boolean = true
  def ast: Ast

  /**
    * Parent scope
    */
  def parent: Option[Scope] = _parent
  private var _parent: Option[Scope] = None
  def parent_=[Child <: Scope](scope: Scope): Unit = {
    if (this._parent.isEmpty) this._parent = Option(scope)
    else throw new UnsupportedOperationException("parent is write-once")
  }

  /**
    * Child scopes, in the order that they appear in the source code
    */
  def children: Seq[Scope] = _children
  private var _children: Seq[Scope] = Seq.empty
  def children_=[Child <: Scope](children: Seq[Child]): Unit = {
    if (this._children.isEmpty) {
      this._children = children
    } else throw new UnsupportedOperationException("children is write-once")
  }

  /**
    * Containing namespace
    */
  def namespace: WdlNamespace = _namespace
  private var _namespace: WdlNamespace = null
  def namespace_=[Child <: WdlNamespace](ns: WdlNamespace): Unit = {
    if (Option(this._namespace).isEmpty) {
      this._namespace = ns
    } else throw new UnsupportedOperationException("namespace is write-once")
  }

  /**
    * Seq(parent, grandparent, great grandparent, ..., WdlNamespace)
    */
  lazy val ancestry: Seq[Scope] = parent match {
    case Some(p) => Seq(p) ++ p.ancestry
    case None => Seq.empty[Scope]
  }

  /**
    * All children ++ children's children ++ etc
    */
  lazy val descendants: Set[Scope] = (children ++ children.flatMap(_.descendants)).toSet

  /**
    * Descendants that are Calls
    */
  lazy val calls: Set[Call] = descendants.collect({ case c: Call => c })

  /**
    * Descendants that are Scatters
    */
  lazy val scatters: Set[Scatter] = descendants.collect({ case s: Scatter => s })

  /**
    * Declarations within this Scope, in the order that they appear in source code
    */
  lazy val declarations: Seq[Declaration] = children.collect({ case d: Declaration => d})

  /**
    * String identifier for this scope.  this.namespace.resolve(this.fullyQualifiedName) == this
    */
  def fullyQualifiedName = {
    (ancestry.reverse.filter(_.appearsInFqn).map(_.unqualifiedName) :+ unqualifiedName).mkString(".")
  }

  /**
    * String identifier for this scope, with hidden scope information.
    *
    * this.namespace.resolve(this.fullyQualifiedNameWithIndexScopes) == this
    */
  def fullyQualifiedNameWithIndexScopes = {
    (Seq(this) ++ ancestry).reverse.map(_.unqualifiedName).filter(_.nonEmpty).mkString(".")
  }

  /**
    * Given another scope, returns the closest common ancestor between the two scopes,
    * if one exists at all
    *
    * @return closest common ancestor
    */
  def closestCommonAncestor(other: Scope): Option[Scope] = {
    val otherAncestry = other.ancestry
    ancestry find { otherAncestry.contains(_) }
  }

  /**
    * Performs scope resolution starting from this scope and walking up the lexical hierarchy
    * until it finds a GraphNode with the `name` as its unqualifiedName
    */
  def resolveVariable(name: String): Option[Scope with GraphNode] = {
    val localLookup = children.collect({ case n: GraphNode => n }) find {
      case s: Scatter => s.item == name
      case n => n.unqualifiedName == name
    }
    localLookup match {
      case scope: Some[_] => scope
      case None => parent.flatMap(_.resolveVariable(name))
    }
  }

  // TODO: sfrazer: move this somewhere, also... duplicate of VariableNotFoundException?
  class VariableLookupException(message: String, cause: Throwable = null) extends Exception(message, cause)

  /**
    * This will return a lookup function for evaluating expressions which will traverse up the
    * scope hierarchy to find a value for `name`.  An exception will be thrown if a value cannot
    * be found for `name`
    *
    * @param scope The scope to start the variable resolution
    * @param inputs All known values of FQNs
    * @param shards For resolving specific shards of scatter blocks
    * @param wdlFunctions Implementation of WDL functions for expression evaluation
    * @return String => WdlValue lookup function rooted at `scope`
    * @throws VariableNotFoundException => If no errors occurred, but also `name` didn't resolve to any value
    * @throws VariableLookupException if anything else goes wrong in looking up a value for `name`
    */
  private def scopeLookupFunction(scope: Scope,
                                  inputs: WorkflowCoercedInputs,
                                  shards: Map[Scatter, Int],
                                  wdlFunctions: WdlFunctions[WdlValue])(name: String): WdlValue = {
    val scopeResolvedValue = scope.resolveVariable(name) match {
      case Some(scatter: Scatter) =>
        // This case will happen if `name` references a Scatter.item (i.e. `x` in expression scatter(x in y) {...})
        val evaluatedCollection = scatter.collection.evaluate(scopeLookupFunction(scatter, inputs, shards, wdlFunctions), wdlFunctions)
        val scatterShard = shards.get(scatter)

        (evaluatedCollection, scatterShard) match {
          case (Success(value: WdlArray), Some(shard)) if 0 <= shard && shard < value.value.size =>
            value.value.lift(shard)
          case (Success(value: WdlArray), Some(shard)) =>
            throw new VariableLookupException(s"Scatter expression (${scatter.collection.toWdlString}) evaluated to an array of ${value.value.size} elements, but element ${shard} was requested.")
          case (Success(value: WdlValue), _) =>
            throw new VariableLookupException(s"Expected scatter expression (${scatter.collection.toWdlString}) to evaluate to an Array.  Instead, got a ${value}")
          case (Failure(ex), _) =>
            throw new VariableLookupException(s"Failed to evaluate scatter expression (${scatter.collection.toWdlString})", ex)
          case (_, None) =>
            throw new VariableLookupException(s"Could not find a shard for scatter block with expression (${scatter.collection.toWdlString})")
        }
      case Some(d: Declaration) if d.expression.isDefined =>
        // TODO: sfrazer: d.parent.get?
        d.expression.get.evaluate(scopeLookupFunction(d.parent.get, inputs, shards, wdlFunctions), wdlFunctions) match {
          case Success(value) => Option(value)
          case Failure(ex) => throw new VariableLookupException(s"Could not evaluate expression for declaration '${d.toWdlString}'", ex)
        }
      case Some(s) => inputs.get(s.fullyQualifiedName)
    }

    if (scopeResolvedValue.isDefined) scopeResolvedValue.get
    else throw new VariableNotFoundException(name)
  }

  def lookupFunction(inputs: WorkflowCoercedInputs,
                     wdlFunctions: WdlFunctions[WdlValue],
                     shards: Map[Scatter, Int] = Map.empty[Scatter, Int]): String => WdlValue = {
    scopeLookupFunction(this, inputs, shards, wdlFunctions)
  }
}
