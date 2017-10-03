package wom.graph

/**
  * Identifies a node in its local context.
  * Used in during graph construction to link nodes together and validate the graph.
  */
case class LocalName(private val value: String) {
  def asString: String = value
  def combineToLocalName(other: String) = LocalName(s"$value.$other")
  def combineToFullyQualifiedName(other: String) = FullyQualifiedName(s"$value.$other")
}

/**
  * Identifies a node uniquely in its graph.
  * This *can* be the same as local name.
  * It is not required by WOM strictly speaking but rather useful to implementations
  * for serializing or reporting.
  * A graph will fail to construct if two or more nodes share the same FullyQualifiedName
  */
case class FullyQualifiedName(private val value: String) {
  def asString: String = value
  def combine(other: String) = FullyQualifiedName(s"$value.$other")
}

object WomIdentifier {
  def apply(localName: String): WomIdentifier = WomIdentifier(LocalName(localName), FullyQualifiedName(localName))
  def apply(localName: String, fullyQualifiedName: String): WomIdentifier = WomIdentifier(LocalName(localName), FullyQualifiedName(fullyQualifiedName))
}

case class WomIdentifier(localName: LocalName, fullyQualifiedName: FullyQualifiedName) {
  def combine(other: String): WomIdentifier = {
    WomIdentifier(localName.combineToLocalName(other), fullyQualifiedName.combine(other))
  }
}
