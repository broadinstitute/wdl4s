package wdl4s

import scala.annotation.tailrec
import scala.language.postfixOps

object Scope {
  /**
   * Collect Calls from a Seq of Scopes.
   * @param scopes scopes to loop through
   * @return Scopes instances that are Calls
   */
  def collectCalls(scopes: Seq[Scope]): Seq[Call] = scopes collect { case s: Call => s }

  /**
   * Collect all Calls from the given scope.
   * @param scopes scope to gather Calls from
   * @param calls for recursivity. Should be passed Nil in most cases.
   * @return all Calls inside the scope
   */
  @tailrec
  def collectAllCalls(scopes: Seq[Scope], calls: Seq[Call]): Seq[Call] = scopes match {
    case Nil => calls
    case l => collectAllCalls(l.flatMap(_.children), calls ++ collectCalls(l))
  }

  /**
   * Collect Scatters from a Seq of Scopes.
   * @param scopes scopes to loop through
   * @return Scopes instances that are Scatters
   */
  def collectScatters(scopes: Seq[Scope]): Seq[Scatter] = scopes collect { case s: Scatter => s }

  /**
   * Collect all Scatters from the given scope.
   * @param scopes scope to gather Scatters from
   * @param scatters for recursivity. Should be passed Nil in most cases.
   * @return all Scatters inside the scope
   */
  @tailrec
  def collectAllScatters(scopes: Seq[Scope], scatters: Seq[Scatter]): Seq[Scatter] = scopes match {
    case Nil => scatters
    case l => collectAllScatters(l.flatMap(_.children), scatters ++ collectScatters(l))
  }

  @tailrec
  def fullyQualifiedNameBuilder(scope: Option[Scope], fqn: String, fullDisplay: Boolean, leaf: Boolean): String = {
    scope match {
      case Some(x: Scope) =>
        fullyQualifiedNameBuilder(
          x.parent,
          (if (fullDisplay || x.appearsInFqn || leaf) s".${x.unqualifiedName}" else "") + fqn,
          fullDisplay,
          leaf = false)
      case None => fqn.tail //Strip away the first "." of the name
    }
  }
}

trait Scope {
  def unqualifiedName: LocallyQualifiedName
  def appearsInFqn: Boolean = true
  def prerequisiteScopes: Set[Scope]
  def prerequisiteCallNames: Set[LocallyQualifiedName]

  def parent: Option[Scope] = _parent
  private var _parent: Option[Scope] = None
  def parent_=[Child <: Scope](scope: Scope): Unit = {
    if (this._parent.isEmpty) this._parent = Option(scope)
    else throw new UnsupportedOperationException("parent is write-once")
  }

  def children: Seq[Scope] = _children
  private var _children: Seq[Scope] = Seq.empty
  def children_=[Child <: Scope](children: Seq[Child]): Unit = {
    if (this._children.isEmpty) {
      this._children = children
    } else throw new UnsupportedOperationException("children is write-once")
  }

  def fullyQualifiedName =
    Scope.fullyQualifiedNameBuilder(Option(this), "", fullDisplay = false, leaf = true)

  def fullyQualifiedNameWithIndexScopes =
    Scope.fullyQualifiedNameBuilder(Option(this), "", fullDisplay = true, leaf = true)

  /**
   * Convenience method to collect Calls from within a scope.
   * @return all calls contained in this scope (recursively)
   */
  private def collectAllCalls = Scope.collectAllCalls(Seq(this), Nil)

  /**
   * Convenience method to collect Scatters from within a scope.
   * @return all scatters contained in this scope (recursively)
   */
  private def collectAllScatters = Scope.collectAllScatters(Seq(this), Nil)

  /**
   * Calls and scatters are accessed frequently so this avoids traversing the whole children tree every time.
   * Lazy because children are not provided at instantiation but rather later during tree building process.
   * This prevents evaluation from being done before children have been set.
   */
  lazy val calls: Seq[Call] = collectAllCalls
  lazy val scatters: Seq[Scatter] = collectAllScatters

  def callByName(callName: String): Option[Call] = calls find { _.unqualifiedName == callName }

  /**
    * Walk the scope hierarchy until the root namespace and return each Scope along the way
    */
  def ancestry: Seq[Scope] = parent match {
    case Some(p) => Seq(p) ++ p.ancestry
    case None => Seq.empty[Scope]
  }

  def closestCommonAncestor(other: Scope): Option[Scope] = {
    val otherAncestry = other.ancestry
    ancestry find { otherAncestry.contains(_) }
  }
}
