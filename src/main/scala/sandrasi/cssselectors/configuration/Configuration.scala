package sandrasi.cssselectors.configuration

import sandrasi.cssselectors.util._

import scala.xml.{MetaData, Node}

// TODO (sandrasi): document the configuration classes
// TODO (sandrasi): Initialize configuration with default and specified namespaces
/**
 * Provides a configuration for [[http://www.w3.org/TR/css3-selectors/ Selectors Level 3]]-compliant selectors applied
 * on a [[scala.xml.NodeSeq NodeSeq]] to select particular [[scala.xml.Node Node]]s.
 *
 * ==Configuration parameters==
 *
 * To configure how the selectors are matched against the document elements a set of parameters are available. If no
 * configuration parameters have been specified, a default configuration is used.
 *
 * ==Document case sensitivity==
 *
 * The case sensitivity of document language element names, attribute names, and attribute values in selectors depends
 * on the document language. For example, in XML, element names are case-sensitive, but in HTML, they are case-insensitive.
 * The `caseSensitiveElementNames`, the `caseSensitiveAttributeNames`, and the `caseSensitiveAttributeValues` parameters
 * control whether or not a selector is matched case sensitively. Namespaces are always case-sensitive.
 *
 * By default every matcher is case-sensitive.
 *
 * @constructor creates a new selector execution context with the specified configuration parameters
 * @param caseSensitiveElementNames a flag to switch case-sensitive matching on element names on or off
 * @param caseSensitiveAttributeNames a flag to switch case-sensitive matching on attribute names on or off
 * @param caseSensitiveAttributeValues a flag to switch case-sensitive matching on attribute values on or off
 */
sealed trait Configuration { outer =>

  def matches(node: Node, namespace: Namespace, name: Option[String]): Boolean = eq(namespace, node.namespace) && name.flatMap(name => Some(eq(node, name))).getOrElse(true)

  def matches(attr: MetaData, owner: Node, namespace: Namespace, name: String, value: String, eqFn: (Option[String], String) => Boolean): Boolean = eq(namespace, attr.getNamespace(owner)) && eq(attr, name) && eq(attr, value, eqFn)

  def withCaseInsensitiveNamespace: Configuration = new Configuration with CaseInsensitiveNamespace {
    override protected def eq(node: Node, name: String) = outer.eq(node, name)
    override protected def eq(attr: MetaData, name: String) = outer.eq(attr, name)
    override protected def eq(attr: MetaData, value: String, eqFn: (Option[String], String) => Boolean) = outer.eq(attr, value, eqFn)
  }

  def withCaseInsensitiveElementNames: Configuration = new Configuration with CaseInsensitiveElementNames {
    override protected def eq(namespace: Namespace, ns: String) = outer.eq(namespace, ns)
    override protected def eq(attr: MetaData, name: String) = outer.eq(attr, name)
    override protected def eq(attr: MetaData, value: String, eqFn: (Option[String], String) => Boolean) = outer.eq(attr, value, eqFn)
  }

  def withCaseInsensitiveAttributeNames: Configuration = new Configuration with CaseInsensitiveAttributeNames {
    override protected def eq(namespace: Namespace, ns: String) = outer.eq(namespace, ns)
    override protected def eq(node: Node, name: String) = outer.eq(node, name)
    override protected def eq(attr: MetaData, value: String, eqFn: (Option[String], String) => Boolean) = outer.eq(attr, value, eqFn)
  }

  def withCaseInsensitiveAttributeValues: Configuration = new Configuration with CaseInsensitiveAttributeValues {
    override protected def eq(namespace: Namespace, ns: String) = outer.eq(namespace, ns)
    override protected def eq(node: Node, name: String) = outer.eq(node, name)
    override protected def eq(attr: MetaData, name: String) = outer.eq(attr, name)
  }

  protected def eq(namespace: Namespace, ns: String): Boolean = namespace match {
    case NoNamespace => ns == null
    case UriNamespace(uri) => uri == ns
    case AnyNamespace => true
    case InvalidNamespace => false
  }

  protected def eq(node: Node, name: String): Boolean = node.label == name

  protected def eq(attr: MetaData, name: String): Boolean = attr.key == name

  protected def eq(attr: MetaData, value: String, eqFn: (Option[String], String) => Boolean): Boolean = eqFn(attr.value.map(_.text).headOption, value)
}

sealed trait CaseInsensitiveNamespace { self: Configuration =>

  override protected def eq(namespace: Namespace, ns: String) = namespace match {
    case NoNamespace => ns == null
    case UriNamespace(uri) => uri.toLowerCase == ns.toLowerCase
    case AnyNamespace => true
    case InvalidNamespace => false
  }
}

sealed trait CaseInsensitiveElementNames { self: Configuration =>

  override protected def eq(node: Node, name: String): Boolean = node.label.equalsIgnoreCase(name)
}

sealed trait CaseInsensitiveAttributeNames { self: Configuration =>

  override protected def eq(attr: MetaData, name: String): Boolean = attr.key.equalsIgnoreCase(name)
}

sealed trait CaseInsensitiveAttributeValues { self: Configuration =>

  override protected def eq(attr: MetaData, value: String, eqFn: (Option[String], String) => Boolean): Boolean = eqFn(attr.value.map(_.text.toLowerCase).headOption, value.toLowerCase)
}

case object DefaultConfiguration extends Configuration
