package sandrasi.cssselectors.selectors

import sandrasi.cssselectors.combinators.Combinator
import sandrasi.cssselectors.configuration.{Configuration, DefaultConfiguration}
import sandrasi.cssselectors.selectors
import sandrasi.cssselectors.selectors.NodeSeqSelector.NodeEnvironment
import sandrasi.cssselectors.util._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.xml._


// TODO (sandrasi): write tests for selectors with invalid namespace
// TODO (sandrasi): replace variable arguments with list and create an apply method with variable arguments om the companinon object instead


/** A common base class for all node sequence selectors. */
sealed trait NodeSeqSelector {

  /** Returns a new node sequence formed from those nodes of `nodeSequence` which are represented by this selector.
    *
    * @param nodeSequence the node sequence from which nodes are selected
    * @param configuration a configuration for node selection
    * @return a sequence of the nodes that are selected by this selector
    */
  def select(nodeSequence: NodeSeq)(implicit configuration: Configuration = DefaultConfiguration): NodeSeq = {

    type UnseenNodes = List[Node]
    type Ancestors = List[Node]
    type VisitedNodes = List[Node]

    val selectedNodes: ListBuffer[Node] = ListBuffer.empty

    @tailrec
    def inOrderSelect(nodes: List[(UnseenNodes, Ancestors, VisitedNodes)]): NodeSeq = nodes match {
      case Nil => NodeSeq.fromSeq(selectedNodes.toList)
      case (Nil, _, _) :: tail => inOrderSelect(tail)
      case (node :: unseenNodes, ancestors, visitedNodes) :: tail =>
        if (isSelected(NodeEnvironment(node, ancestors, visitedNodes, unseenNodes))(configuration)) {
          selectedNodes += node
        }
        inOrderSelect((elemsOf(node.child), node :: ancestors, Nil) :: (unseenNodes, ancestors, node :: visitedNodes) :: tail)
    }

    def elemsOf(ns: NodeSeq) = ns.view.filter(_.isInstanceOf[Elem]).toList

    inOrderSelect(List((elemsOf(nodeSequence), Nil, Nil)))
  }

  /** Returns a selector that is equivalent to this selector and that may contain fewer components.
    *
    * @return  a simpler selector that represents the same nodes as this selector. Returns this selector if no
    *          simplification is possible. Returns [[sandrasi.cssselectors.selectors.InvalidSelector InvalidSelector]]
    *          if any part of this selector is invalid.
    */
  def simplify: NodeSeqSelector

  /** Decides if a node is selected based on the selection rules of this selector.
    *
    * @param nodeEnv the node to be tested and its relative nodes
    * @param configuration a configuration for node selection
    * @return `true` if `node` is selected, `false` otherwise
    */
  protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean
}
object NodeSeqSelector {

  /** A data structure representing a node with its parent and sibling nodes.
    *
    * @param node a node in a node sequence
    * @param ancestors The ancestor nodes of `node`. The first element of the list is the parent of `node`, the second
    *                  is the grandparent of `node`, the third is the grand-grandparent of `node`, and so on all the way
    *                  up to the root node.
    * @param before The sibling nodes before `node`. Sibling nodes share the same parent. The first element of the list
    *               is the node adjacent to `node`, the second is the node before that, and so on all the way back to
    *               the first node under the same parent node.
    * @param after The sibling nodes after `node`. Sibling nodes share the same parent. The first element of the list is
    *              the node adjacent to `node`, the second is the node after that, and so on all the way to the last
    *              node under the same parent node.
    */
  private[selectors] case class NodeEnvironment(node: Node, ancestors: List[Node], before: List[Node], after: List[Node])
}

sealed trait SelectorGroup extends NodeSeqSelector {
  override def simplify: SelectorGroup
}

case class SelectorsGroupSelector(selectorCombination: SelectorCombination, selectorCombinations: SelectorCombination*) extends SelectorGroup {
  require(selectorCombination != null, "selectorCombination is required")
  require(selectorCombinations != null, "selectorCombinations is required")
  // TODO (sandrasi): temporary implementation so that the selector parser tests pass
  override def simplify: SelectorGroup = {
    if (selectorCombinations.isEmpty) {
      selectorCombination.simplify
    } else if (selectorCombination.simplify == InvalidSelector) {
      InvalidSelector
    } else if (selectorCombinations.exists(_.simplify == InvalidSelector)) {
      InvalidSelector
    } else {
      SelectorsGroupSelector(selectorCombination.simplify, selectorCombinations.map(_.simplify):_*)
    }
  }
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

sealed trait SelectorCombination extends SelectorGroup {
  override def simplify: SelectorCombination
}

case class Selector(selectorSequence: SelectorSequence, combinedSelectorSequences: (Combinator, SelectorSequence)*) extends SelectorCombination {
  require(selectorSequence != null, "simpleSelectorSequence is required")
  require(combinedSelectorSequences != null, "combinedSelectorSequences is required")
  // TODO (sandrasi): temporary implementation so that the selector parser tests pass
  override def simplify: SelectorCombination = {
    if (combinedSelectorSequences.isEmpty) {
      selectorSequence.simplify
    } else if (selectorSequence.simplify == InvalidSelector) {
      InvalidSelector
    } else if (combinedSelectorSequences.exists { case (combinator, ss) => ss.simplify == InvalidSelector }) {
      InvalidSelector
    } else {
      Selector(selectorSequence.simplify, combinedSelectorSequences.map { case (combinator, ss) => (combinator, ss.simplify) }:_*)
    }
  }
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

sealed trait SelectorSequence extends SelectorCombination {
  override def simplify: SelectorSequence
}

case class SimpleSelectorSequenceSelector(
  elementSelector: Option[ElementSelector],
  chainableSelectors: ChainableSelector*
)(
  pseudoElementSelector: Option[PseudoElementSelector] = None
) extends SelectorSequence {

  require(elementSelector != null, "elementSelector is required")
  require(chainableSelectors.forall(_ != null), "chainableSelectors should not contain null elements")
  require(pseudoElementSelector != null, "pseudoElementSelector is required")
  require(
    elementSelector.isDefined || chainableSelectors.size > 0 || pseudoElementSelector.isDefined,
    "at least one selector is required"
  )

  /** @inheritdoc */
  override def simplify: SelectorSequence = (elementSelector, chainableSelectors, pseudoElementSelector) match {
    case (Some(es), cs, Some(pes)) => if (isValid(cs :+ es :+ pes:_*)) this else InvalidSelector
    case (Some(es), cs, None) => if (cs.isEmpty) es.simplify else if (isValid(cs:_*)) this else InvalidSelector
    case (None, cs, Some(pes)) => if (cs.isEmpty) pes.simplify else if (isValid(cs :+ pes:_*)) this else InvalidSelector
    case (_, cs, _) => if (cs.length == 1) cs.head.simplify else if (isValid(cs:_*)) this else InvalidSelector
  }

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    val isSelected: (NodeSeqSelector) => Boolean = _.isSelected(nodeEnv)(configuration)
    elementSelector.forall(isSelected) &&
      chainableSelectors.forall(isSelected) &&
      pseudoElementSelector.forall(isSelected)
  }

  private def isValid(nodeSeqSelectors: NodeSeqSelector*) = nodeSeqSelectors.forall(_.simplify != InvalidSelector)
}
case object SimpleSelectorSequenceSelector {

  def apply(elementSelector: ElementSelector): SimpleSelectorSequenceSelector = {
    SimpleSelectorSequenceSelector(Option(elementSelector))()
  }

  def apply(chainableSelectors: ChainableSelector*): SimpleSelectorSequenceSelector = {
    SimpleSelectorSequenceSelector(None, chainableSelectors:_*)()
  }

  def apply(pseuodeElementSelector: PseudoElementSelector): SimpleSelectorSequenceSelector = {
    SimpleSelectorSequenceSelector(None)(Option(pseuodeElementSelector))
  }

  def apply(elementSelector: ElementSelector, chainableSelectors: ChainableSelector*): SimpleSelectorSequenceSelector = {
    SimpleSelectorSequenceSelector(Option(elementSelector), chainableSelectors:_*)()
  }

  def apply(elementSelector: ElementSelector, pseudoElementSelector: PseudoElementSelector): SimpleSelectorSequenceSelector = {
    SimpleSelectorSequenceSelector(Option(elementSelector))(Option(pseudoElementSelector))
  }
}

/** Base class for selectors that can be the argument of [[sandrasi.cssselectors.selectors.NotSelector NotSelector]]. */
sealed trait NegatableSelector extends NodeSeqSelector {

  /** @inheritdoc */
  override def simplify: NegatableSelector
}

/** Base class for selectors that match on elements. */
sealed trait ElementSelector extends SelectorSequence with NegatableSelector {

  /** @inheritdoc */
  override def simplify: ElementSelector
}

/** Represents the CSS3 universal selector. The universal selector represents any single element in the document tree in
  * the specified or in any namespace (including those without a namespace).
  *
  * ==Universal selector and namespaces==
  *
  * The universal selector takes a `namespace` argument. If `namespace` is
  * [[sandrasi.cssselectors.util.NoNamespace NoNamespace]], this universal selector represents those elements that are
  * not in any namespace. If `namespace` is [[sandrasi.cssselectors.util.AnyNamespace AnyNamespace]], this universal
  * selector represents all elements in all namespaces. Otherwise, if namespace is
  * [[sandrasi.cssselectors.util.UriNamespace UriNamespace]], it represents those elements that are in the specified
  * namespace. If the namespace is invalid, this universal selector represents no elements.
  *
  *  ==Usage==
  *
  *  {{{
  *    val xml = <root>
  *                <elem1 />
  *                <elem2 />
  *              </root>
  *    val selected = UniversalSelector(AnyNamespace).select(xml)
  *  }}}
  *
  *  In this example the value of the `selected` variable is going to be the node sequence
  *  `<root><elem1 /><elem2 /></root><elem1 /><elem2 />` because the selector matches on all elements in all namespaces.
  *
  * @constructor creates a new universal selector with the specified namespace
  * @param namespace the namespace this universal selector matches elements in
  * @throws IllegalArgumentException if `namespace` is `null`
  */
case class UniversalSelector(namespace: Namespace) extends ElementSelector {

  require(namespace != null, "namespace is required")

  /** @inheritdoc */
  override def simplify: ElementSelector = if (namespace != InvalidNamespace) this else InvalidSelector

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    configuration.matches(nodeEnv.node, namespace, None)
  }
}
/** Factory for [[sandrasi.cssselectors.selectors.UniversalSelector UniversalSelector]] instances. */
case object UniversalSelector {

  /** Creates a new universal selector that matches on all elements in all namespaces. */
  def apply(): UniversalSelector = UniversalSelector(AnyNamespace)

  /** Creates a new universal selector that matches on all elements in the specified namespace. If `namespaceUri` is
    * `null` the universal selector will match on those elements that are not in any namespace.
    */
  def apply(namespaceUri: String): UniversalSelector = {
    UniversalSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri))
  }
}

/** Represents the CSS3 type selector. A type selector represents all instances of the element type in the document tree
  * in the specified or in any namespace (including those without a namespace).
  *
  * ==Type selectors and namespaces==
  *
  * The type selector takes a `namespace` argument. If `namespace` is
  * [[sandrasi.cssselectors.util.NoNamespace NoNamespace]], this type selector represents those instances of the
  * specified element type that are not in any namespace. If namespace is
  * [[sandrasi.cssselectors.util.AnyNamespace AnyNamespace]], this type selector represents all instances of the
  * specified element type in all namespaces. Otherwise, if namespace is
  * [[sandrasi.cssselectors.util.UriNamespace UriNamespace]], it represents those instances of the specified element
  * type that are in the specified namespace. If the namespace is invalid, this type selector represents no elements.
  *
  *  ==Usage==
  *
  *  {{{
  *    val xml = <root>
  *                <elem1 />
  *                <elem2 />
  *              </root>
  *    val selected = TypeSelector(AnyNamespace, "elem1").select(xml)
  *  }}}
  *
  *  In this example the value of the `selected` variable is going to be `<elem1 />` because the type selector matches
  *  on the `elem1` elements in all namespaces.
  *
  * @constructor creates a new type selector with the specified namespace and element type name
  * @param namespaceMatcher the namespace this type selector matches elements in
  * @param name the name of the element this type selector matches on
  * @throws IllegalArgumentException if either `namespace` or `name` is `null`
  */
case class TypeSelector(namespace: Namespace, name: String) extends ElementSelector {

  require(namespace != null, "namespace is required")
  require(name != null, "name is required")

  /** @inheritdoc */
  override def simplify: ElementSelector = if (namespace != InvalidNamespace) this else InvalidSelector

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    configuration.matches(nodeEnv.node, namespace, Some(name))
  }
}
/** Factory for [[sandrasi.cssselectors.selectors.TypeSelector TypeSelector]] instances. */
case object TypeSelector {

  /** Creates a new type selector that matches on the instances of the specified element type in all namespaces. */
  def apply(name: String): TypeSelector = TypeSelector(AnyNamespace, name)

  /** Creates a new type selector that matches on the instances of the specified element type in the specified
    * namespace. If `namespaceUri` is `null` the type selector will match on those instances of the specified element
    * type that are not in any namespace.
    */
  def apply(namespaceUri: String, name: String): TypeSelector = {
    TypeSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name)
  }
}

/** A common base class for selectors that can be chained after each other in a simple selector sequence. */
sealed trait ChainableSelector extends SelectorSequence {

  /** @inheritdoc */
  override def simplify: ChainableSelector
}

/** Represents the CSS3 ID selector. An ID selector represents an element instance in the document tree that has an `id`
  *  attribute that is not in any namespace and the attribute's value is exactly the specified identifier.
  *
  *  ==Usage==
  *
  *  {{{
  *    val xml = <root>
  *                <elem id="identifier" />
  *                <elem id="something else" />
  *              </root>
  *    val selected = IdSelector("identifier").select(xml)
  *  }}}
  *
  *  In this example the value of the `selected` variable is going to be `<elem id="identifier" />` because this element
  *  has an `id` attribute with a value that exactly matches the `identifier` string.
  *
  * @constructor creates a new ID selector with the specified identifier
  * @param identifier the identifier this ID selector matches on
  * @throws IllegalArgumentException if `identifier` is `null`
  */
case class IdSelector(identifier: String) extends ChainableSelector with NegatableSelector {

  require(identifier != null, "identifier is required")

  /** @inheritdoc */
  override def simplify: IdSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    ExactAttributeSelector(NoNamespace, "id", identifier).isSelected(nodeEnv)(configuration)
  }
}

/** Represents the CSS3 class selector. A class selector represents those elements in the document tree that have a
  * `class` attribute that is not in any namespace and the attribute's value is a whitespace-separated list of words,
  * one of which is exactly the specified class.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elem class="class1 class2" />
  *               <elem class="class3" />
  *             </root>
  *   val selected = ClassSelector("class1").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem class="class1 class2" />` because this
  * element has a `class` attribute with a value of whitespace-separated list of words from which one is exactly
  * `class1`.
  *
  * @constructor creates a new class selector with the specified class name
  * @param className the name of the class this class selector matches on
  * @throws IllegalArgumentException if `className` is `null`
  */
case class ClassSelector(className: String) extends ChainableSelector with NegatableSelector {

  require(className != null, "className is required")

  /** @inheritdoc */
  override def simplify: ClassSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    IncludesAttributeSelector(NoNamespace, "class", className).isSelected(nodeEnv)(configuration)
  }
}

/** A common base class for the CSS3 attribute presence and value selectors which may be invalid. */
sealed trait BaseAttributeSelector extends ChainableSelector with NegatableSelector {

  override def simplify: BaseAttributeSelector
}

/** A common base class for the CSS3 attribute presence and value selectors.
  *
  * ==Attribute selectors and namespaces==
  *
  * An attribute selector takes a `namespace` argument. If `namespace` is
  * [[sandrasi.cssselectors.util.NoNamespace NoNamespace]], this attribute selector represents those elements that have
  * the specified attribute that is not in any namespace with the specified value. If namespace is
  * [[sandrasi.cssselectors.util.AnyNamespace AnyNamespace]], this attribute selector represents those elements that
  * have the specified attribute in any namespace with the specified value. Otherwise, if namespace is
  * [[sandrasi.cssselectors.util.UriNamespace UriNamespace]], it represents those elements that have the specified
  * attribute in the specified namespace with the specified value. If the namespace is invalid, this attribute selector
  * represents no elements.
  *
  * ==Default attribute values in external subsets==
  *
  * How a document tree is constructed is outside the scope of selectors. In some document formats default attribute
  * values can be defined in a DTD or elsewhere, but these can only be selected by attribute selectors if they appear in
  * the document tree.
  */
sealed trait AttributeSelector extends BaseAttributeSelector {

  require(namespace != null, "namespace is required")
  require(name != null, "name is required")
  require(value != null, "value is required")

  /** The namespace this attribute selector matches attributes in */
  def namespace: Namespace

  /** The name of the attribute this attribute selector matches on */
  def name: String

  /** The value of the attribute this attribute selector matches on */
  def value: String

  /** The value matcher used to match on attribute values */
  protected def valueMatcher(attrValue: Option[String], value: String): Boolean

  /** @inheritdoc */
  override def simplify: BaseAttributeSelector = if (namespace != InvalidNamespace) this else InvalidSelector

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    nodeEnv.node.attributes.exists { a => configuration.matches(a, nodeEnv.node, namespace, name, value, valueMatcher) }
  }
}

/** Represents the CSS3 attribute presence selector. An attribute presence selector represents those elements in the
  * document tree which have an attribute in the specified or in any namespace (including those without a namespace)
  * with the specified value.
  *
  * ===Usage===
  *
  * {{{
  *   val xml = <root>
  *               <elem1 attr="value" />
  *               <elem2 />
  *             </root>
  *   val selected = AttributePresenceSelector(AnyNamespace, "attr").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 attr="value" />` because this element
  * has an `attr` attribute.
  *
  * @constructor creates an attribute presence selector with the specified namespace and attribute name
  * @param namespace the namespace this attribute selector matches elements in
  * @param name the name of the attribute this attribute selector matches on
  * @throws IllegalArgumentException if either `namespace` or `name` is `null`
  */
case class AttributePresenceSelector(namespace: Namespace, name: String) extends AttributeSelector {

  /** @inheritdoc */
  override def value: String = ""

  /** @inheritdoc */
  override protected def valueMatcher(attrValue: Option[String], value: String): Boolean = true
}
/** Factory for [[sandrasi.cssselectors.selectors.AttributePresenceSelector AttributePresenceSelector]] instances. */
case object AttributePresenceSelector {

  /** Creates a new attribute presence selector that matches on the specified attribute in all namespaces. */
  def apply(name: String): AttributePresenceSelector = AttributePresenceSelector(AnyNamespace, name)

  /** Creates a new attribute presence selector that matches on the specified attribute in the specified namespace. If
    * `namespaceUri` is `null` the attribute presence elector will match on those specified attributes that are not in
    * any namespace.
    */
  def apply(namespaceUri: String, name: String): AttributePresenceSelector = {
    AttributePresenceSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name)
  }
}

/** Represents the CSS3 attribute value selector with the ''dash'' value matcher. A dash attribute selector represents
  * all elements in the document tree that have the specified attribute in the specified namespace and the attribute's
  * value is either being exactly the specified value or beginning with the specified value immediately followed by
  * ''-'' (U+002D).
  *
  * ===Usage===
  *
  * {{{
  *   val xml = <root>
  *               <elem1 attr="value" />
  *               <elem2 attr="value-something" />
  *               <elem3 />
  *             </root>
  *   val selected = DashAttributeSelector(AnyNamespace, "attr", "value").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be
  * `<elem1 attr="value" /><elem2 attr="value-something" />` because these elements have an `attr` attribute with a
  * value that is exactly `value` or starts with `value` and is immediately followed by a `-`.
  *
  * @constructor creates a dash attribute selector with the specified namespace, attribute name, and attribute value
  * @param namespace the namespace this attribute selector matches elements in
  * @param name the name of the attribute this attribute selector matches on
  * @param value the value this attribute selector looks for in the attribute's value
  * @throws IllegalArgumentException if either `namespace`, `name`, or `value` is `null`
  */
case class DashAttributeSelector(namespace: Namespace, name: String, value: String) extends AttributeSelector {

  /** @inheritdoc */
  override protected def valueMatcher(attrValue: Option[String], value: String): Boolean = {
    attrValue.contains(value) || attrValue.exists(_.startsWith(s"$value-"))
  }
}
/** Factory for [[sandrasi.cssselectors.selectors.DashAttributeSelector DashAttributeSelector]] instances. */
case object DashAttributeSelector {

  /** Creates a new dash attribute selector that matches on the specified attribute with the specified value in all
    * namespaces.
    */
  def apply(name: String, value: String): DashAttributeSelector = DashAttributeSelector(AnyNamespace, name, value)

  /** Creates a new dash attribute selector that matches on the specified attribute with the specified value in the
    * specified namespace. If `namespaceUri` is `null` the dash attribute selector will match on those specified
    * attributes with the specified value that are not in any namespace.
    */
  def apply(namespaceUri: String, name: String, value: String): DashAttributeSelector = {
    DashAttributeSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name, value)
  }
}

/** Represents the CSS3 attribute value selector with the ''exact'' value matcher. An exact attribute selector
  * represents all elements in the document tree that have the specified attribute in the specified namespace and the
  * attribute's value is exactly the specified value.
  *
  * ===Usage===
  *
  * {{{
  *   val xml = <root>
  *               <elem1 attr="value" />
  *               <elem2 attr="other value" />
  *             </root>
  *   val selected = ExactAttributeSelector(AnyNamespace, "attr", "value").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 attr="value" /> because this element has
  * an `attr` attribute with a value that is exactly `value`.
  *
  * @constructor creates an exact attribute selector with the specified namespace, attribute name, and attribute value
  * @param namespace the namespace this attribute selector matches elements in
  * @param name the name of the attribute this attribute selector matches on
  * @param value the value this attribute selector looks for in the attribute's value
  * @throws IllegalArgumentException if either `namespace`, `name`, or `value` is `null`
  */
case class ExactAttributeSelector(namespace: Namespace, name: String, value: String) extends AttributeSelector {

  /** @inheritdoc */
  override protected def valueMatcher(attrValue: Option[String], value: String): Boolean = attrValue.contains(value)
}
/** Factory for [[sandrasi.cssselectors.selectors.ExactAttributeSelector ExactAttributeSelector]] instances. */
case object ExactAttributeSelector {

  /** Creates a new exact attribute selector that matches on the specified attribute with the specified value in all
    * namespaces.
    */
  def apply(name: String, value: String): ExactAttributeSelector = ExactAttributeSelector(AnyNamespace, name, value)

  /** Creates a new exact attribute selector that matches on the specified attribute with the specified value in the
    * specified namespace. If `namespaceUri` is `null` the exact attribute selector will match on those specified
    * attributes with the specified value that are not in any namespace.
    */
  def apply(namespaceUri: String, name: String, value: String): ExactAttributeSelector = {
    ExactAttributeSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name, value)
  }
}

/** Represents the CSS3 attribute value selector with the ''includes'' value matcher. An includes attribute selector
  * represents all elements in the document tree that have the specified attribute in the specified namespace and the
  * attribute's value is a whitespace-separated list of words, one of which is exactly the specified value. If the
  * specified value contains whitespace, this selector does not represent anything (since the words are separated by
  * spaces). Also if the specified value is the empty string, this attribute selector does not represent anything.
  *
  * ===Usage===
  *
  * {{{
  *   val xml = <root>
  *               <elem1 attr="includes the value word" />
  *               <elem2 attr="does not include the specified word" />
  *             </root>
  *   val selected = IncludesAttributeSelector(AnyNamespace, "attr", "value").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 attr="includes the value word" />
  * because this element has an `attr` attribute with a value of whitespace-separated list of words, one of which is
  * exactly `value`.
  *
  * @constructor creates an includes attribute selector with the specified namespace, attribute name, and attribute
  *              value
  * @param namespace the namespace this attribute selector matches elements in
  * @param name the name of the attribute this attribute selector matches on
  * @param value the value this attribute selector looks for in the attribute's value
  * @throws IllegalArgumentException if either `namespace`, `name`, or `value` is `null`
  */
case class IncludesAttributeSelector(namespace: Namespace, name: String, value: String) extends AttributeSelector {

  /** @inheritdoc */
  override protected def valueMatcher(attrValue: Option[String], value: String): Boolean = {
    value.nonEmpty && attrValue.exists(_.split("""\s+""").contains(value))
  }
}
/** Factory for [[sandrasi.cssselectors.selectors.IncludesAttributeSelector IncludesAttributeSelector]] instances. */
case object IncludesAttributeSelector {

  /** Creates a new includes attribute selector that matches on the specified attribute with the specified value in all
    * namespaces.
    */
  def apply(name: String, value: String): IncludesAttributeSelector = {
    IncludesAttributeSelector(AnyNamespace, name, value)
  }

  /** Creates a new includes attribute selector that matches on the specified attribute with the specified value in the
    * specified namespace. If `namespaceUri` is `null` the includes attribute selector will match on those specified
    * attributes with the specified value that are not in any namespace.
    */
  def apply(namespaceUri: String, name: String, value: String): IncludesAttributeSelector = {
    IncludesAttributeSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name, value)
  }
}

/** Represents the CSS3 attribute value selector with the ''prefix'' value matcher. A prefix attribute selector
  * represents all elements in the document tree that have the specified attribute in the specified namespace and the
  * attribute's value begins with the specified value. If the specified value is the empty string, this attribute
  * selector does not represent anything.
  *
  * ===Usage===
  *
  * {{{
  *   val xml = <root>
  *               <elem1 attr="value prefix" />
  *               <elem2 attr="the value is not a prefix" />
  *             </root>
  *   val selected = PrefixAttributeSelector(AnyNamespace, "attr", "value").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 attr="value prefix" /> because this
  * element has an `attr` attribute of which value begins with `value` prefix.
  *
  * @constructor creates a prefix attribute selector with the specified namespace, attribute name, and attribute value
  * @param namespace the namespace this attribute selector matches elements in
  * @param name the name of the attribute this attribute selector matches on
  * @param value the value this attribute selector looks for in the attribute's value
  * @throws IllegalArgumentException if either `namespace`, `name`, or `value` is `null`
  */
case class PrefixAttributeSelector(namespace: Namespace, name: String, value: String) extends AttributeSelector {

  /** @inheritdoc */
  override protected def valueMatcher(attrValue: Option[String], value: String): Boolean = {
    value.nonEmpty && attrValue.exists(_.startsWith(value))
  }
}
/** Factory for [[sandrasi.cssselectors.selectors.PrefixAttributeSelector PrefixAttributeSelector]] instances. */
case object PrefixAttributeSelector {

  /** Creates a new prefix attribute selector that matches on the specified attribute with the specified value in all
    * namespaces.
    */
  def apply(name: String, value: String): PrefixAttributeSelector = PrefixAttributeSelector(AnyNamespace, name, value)

  /** Creates a new prefix attribute selector that matches on the specified attribute with the specified value in the
    * specified namespace. If `namespaceUri` is `null` the prefix attribute selector will match on those specified
    * attributes with the specified value that are not in any namespace.
    */
  def apply(namespaceUri: String, name: String, value: String): PrefixAttributeSelector = {
    PrefixAttributeSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name, value)
  }
}

/** Represents the CSS3 attribute value selector with the ''substring'' value matcher. A substring attribute selector
  * represents all elements in the document tree that have the specified attribute in the specified namespace and the
  * attribute's value contains at least one instance of the specified value. If the specified value is the empty string,
  * this attribute selector does not represent anything.
  *
  * ===Usage===
  *
  * {{{
  *   val xml = <root>
  *               <elem1 attr="contains the value substring" />
  *               <elem2 attr="doesn't contain the specified word" />
  *             </root>
  *   val selected = SubstringAttributeSelector(AnyNamespace, "attr", "value").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 attr="contains the value substring" />
  * because this element has an `attr` attribute of which value contains the `value` substring.
  *
  * @constructor creates a substring attribute selector with the specified namespace, attribute name, and attribute
  *              value
  * @param namespace the namespace this attribute selector matches elements in
  * @param name the name of the attribute this attribute selector matches on
  * @param value the value this attribute selector looks for in the attribute's value
  * @throws IllegalArgumentException if either `namespace`, `name`, or `value` is `null`
  */
case class SubstringAttributeSelector(namespace: Namespace, name: String, value: String) extends AttributeSelector {

  /** @inheritdoc */
  override protected def valueMatcher(attrValue: Option[String], value: String): Boolean = {
    value.nonEmpty && attrValue.exists(_.contains(value))
  }
}
/** Factory for [[sandrasi.cssselectors.selectors.SubstringAttributeSelector SubstringAttributeSelector]] instances. */
case object SubstringAttributeSelector {

  /** Creates a new substring attribute selector that matches on the specified attribute with the specified value in all
    * namespaces.
    */
  def apply(name: String, value: String): SubstringAttributeSelector = {
    SubstringAttributeSelector(AnyNamespace, name, value)
  }

  /** Creates a new substring attribute selector that matches on the specified attribute with the specified value in the
    * specified namespace. If `namespaceUri` is `null` the substring attribute selector will match on those specified
    * attributes with the specified value that are not in any namespace.
    */
  def apply(namespaceUri: String, name: String, value: String): SubstringAttributeSelector = {
    SubstringAttributeSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name, value)
  }
}

/** Represents the CSS3 attribute value selector with the ''suffix'' value matcher. A suffix attribute selector
  * represents all elements in the document tree that have the specified attribute in the specified namespace and the
  * attribute's value ends with the specified value. If the specified value is the empty string, this attribute selector
  * does not represent anything.
  *
  * ===Usage===
  *
  * {{{
  *   val xml = <root>
  *               <elem1 attr="suffix value" />
  *               <elem2 attr="the value is not a suffix" />
  *             </root>
  *   val selected = SuffixAttributeSelector(AnyNamespace, "attr", "value").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 attr="suffix value" /> because this
  * element has an `attr` attribute of which value ends with the `value` suffix.
  *
  * @constructor creates a suffix attribute selector with the specified namespace, attribute name, and attribute value
  * @param namespace the namespace this attribute selector matches elements in
  * @param name the name of the attribute this attribute selector matches on
  * @param value the value this attribute selector looks for in the attribute's value
  * @throws IllegalArgumentException if either `namespace`, `name`, or `value` is `null`
  */
case class SuffixAttributeSelector(namespace: Namespace, name: String, value: String) extends AttributeSelector {

  /** @inheritdoc */
  override protected def valueMatcher(attrValue: Option[String], value: String): Boolean = {
    value.nonEmpty && attrValue.exists(_.endsWith(value))
  }
}
/** Factory for [[sandrasi.cssselectors.selectors.SuffixAttributeSelector SuffixAttributeSelector]] instances. */
case object SuffixAttributeSelector {

  /** Creates a new suffix attribute selector that matches on the specified attribute with the specified value in all
    * namespaces.
    */
  def apply(name: String, value: String): SuffixAttributeSelector = SuffixAttributeSelector(AnyNamespace, name, value)

  /** Creates a new suffix attribute selector that matches on the specified attribute with the specified value in the
    * specified namespace. If `namespaceUri` is `null` the suffix attribute selector will match on those specified
    * attributes with the specified value that are not in any namespace.
    */
  def apply(namespaceUri: String, name: String, value: String): SuffixAttributeSelector = {
    SuffixAttributeSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), name, value)
  }
}

/** A common base class for selectors that permit element selection based on characteristics other than their name,
  * attributes or content; in principle characteristics that cannot be deduced from the document tree.
  */
sealed trait PseudoClassSelector extends ChainableSelector with NegatableSelector {

  /** @inheritdoc */
  override def simplify: PseudoClassSelector
}

/** Represents the CSS3 :root structural pseudo-class selector. The :root selector represents an element that is the
  * root of the document.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elem />
  *             </root>
  *   val selected = RootSelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<root><elem /></root>`.
  *
  * @constructor creates a new :root selector
  */
case class RootSelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: RootSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    nodeEnv.ancestors.isEmpty
  }
}

/** Represents the CSS3 :first-child structural pseudo-class selector. A :first-child selector represents those elements
  * in the document tree that are the first child of some other element. Selects the same elements as
  * `NthChildSelector(0, 1)`.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <firstChild />
  *               <secondChild />
  *             </root>
  *   val selected = FirstChildSelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<firstChild />` because it is the first child of
  * its parent element, `<root>`.
  *
  * @constructor creates a new :first-child selector
  */
case class FirstChildSelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: FirstChildSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    NthChildSelector(0, 1).isSelected(nodeEnv)(configuration)
  }
}

/** Represents the CSS3 :last-child structural pseudo-class selector. A :last-child selector represents those elements
  * in the document tree that are the last child of some other element. Selects the same elements as
  * `NthLastChildSelector(0, 1)`.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <firstChild />
  *               <secondChild />
  *             </root>
  *   val selected = LastChildSelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<secondChild />` because it is the last child
  * of its parent element, `<root>`.
  *
  * @constructor creates a new :last-child selector
  */
case class LastChildSelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: LastChildSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    NthLastChildSelector(0, 1).isSelected(nodeEnv)(configuration)
  }
}

/** Represents the CSS3 :first-of-type structural pseudo-class selector. A :first-of-type selector represents those
  * elements in the document tree that are the first sibling of their type in the list of children of some other
  * element. Selects the same elements as `NthOfTypeSelector(0, 1)`.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elemType1 idx="1" />
  *               <elemType2 idx="1" />
  *               <elemType1 idx="2" />
  *               <elemType2 idx="2" />
  *             </root>
  *   val selected = FirstOfTypeSelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elemType1 idx="1" /><elemType2 idx="1" />`
  * because they are the first sibling of their type in the list of children of their parent element, `<root>`.
  *
  * @constructor creates a new :first-of-type selector
  */
case class FirstOfTypeSelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: FirstOfTypeSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    NthOfTypeSelector(0, 1).isSelected(nodeEnv)(configuration)
  }
}

/** Represents the CSS3 :last-of-type structural pseudo-class selector. A :last-of-type selector represents those
  * elements in the document tree that are the last sibling of their type in the list of children of some other element.
  * Selects the same elements as `NthLastOfTypeSelector(0, 1)`.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elemType1 idx="1" />
  *               <elemType2 idx="1" />
  *               <elemType1 idx="2" />
  *               <elemType2 idx="2" />
  *             </root>
  *   val selected = LastOfTypeSelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elemType1 idx="2" /><elemType2 idx="2" />`
  * because they are the last sibling of their type in the list of children of their parent element, `<root>`.
  *
  * @constructor creates a new :last-of-type selector
  */
case class LastOfTypeSelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: LastOfTypeSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    NthLastOfTypeSelector(0, 1).isSelected(nodeEnv)(configuration)
  }
}

/** Represents the CSS3 :only-child structural pseudo-class selector. An :only-child selector represents those elements
  * in the document tree that are the children of some other elements and their parent element has no other children.
  * Selects the same elements as `SimpleSelectorSequenceSelector(FirstChildSelector(), LastChildSelector())` or
  * `SimpleSelectorSequenceSelector(NthChildSelector(0, 1), NthLastChildSelector(0, 1))`.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elem />
  *             </root>
  *   val selected = OnlyChildSelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem /> because it is the only child of its
  * parent element, `<root>`.
  *
  * @constructor creates a new :only-child selector
  */
case class OnlyChildSelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: OnlyChildSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    SimpleSelectorSequenceSelector(FirstChildSelector(), LastChildSelector()).isSelected(nodeEnv)(configuration)
  }
}

/** Represents the CSS3 :only-of-type structural pseudo-class selector. An :only-of-type selector represents those
  * elements in the document tree that are the children of some other elements and their parent element has no other
  * children with the same expanded element name. Selects the same elements as
  * `SimpleSelectorSequenceSelector(FirstOfTypeSelector(), LastOfTypeSelector())` or
  * `SimpleSelectorSequenceSelector(NthOfTypeSelector(0, 1), NthLastOfTypeSelector(0, 1))`.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elem1 />
  *               <elem2 />
  *             </root>
  *   val selected = OnlyOfTypeSelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 /><elem2 /> because they are the only
  * child of their type of their parent element, `<root>`.
  *
  * @constructor creates a new :only-of-type selector
  */
case class OnlyOfTypeSelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: OnlyOfTypeSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    SimpleSelectorSequenceSelector(FirstOfTypeSelector(), LastOfTypeSelector()).isSelected(nodeEnv)(configuration)
  }
}

/** Represents the CSS3 :empty structural pseudo-class selector. The :empty selector represents those elements in the
  * document tree that have no children at all. In terms of the document tree, only element nodes and content nodes
  * (such as DOM &#91;[[http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407 Document Object Model (DOM) Level 3 Core
  * Specification]]&#93; text nodes, CDATA nodes, and entity references) whose data has a non-zero length are considered
  * as affecting emptiness; comments, processing instructions, and other nodes do not affect whether an element is
  * considered empty or not.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elem1 />
  *               <elem2>text</elem2>
  *             </root>
  *   val selected = EmptySelector().select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 /> because it is the only element that
  * has no child elements at all.
  *
  * @constructor creates a new :empty selector
  */
case class EmptySelector() extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: EmptySelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    nodeEnv.node.child.filter { childNode => childNode.isInstanceOf[Elem] || childNode.text.nonEmpty }.isEmpty
  }
}

case class LinkSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class VisitedSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class ActiveSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class HoverSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class FocusSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class TargetSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class EnabledSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class DisabledSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class CheckedSelector() extends PseudoClassSelector {
  override def simplify: PseudoClassSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

/** A common base class for structural pseudo-classes that permit element selection based on a function's result. */
sealed trait FunctionalPseudoClassSelector extends PseudoClassSelector {

  /** @inheritdoc */
  override def simplify: FunctionalPseudoClassSelector
}

/** A common base class for structural pseudo-classes that permit element selection based on the element's position in
  * the document tree using the ''a''n + ''b'' formula.
  */
sealed trait NumberExpressionSelector extends FunctionalPseudoClassSelector {

  /** The cycle size. */
  def a: Int

  /** The offset value. */
  def b: Int

  /** @inheritdoc */
  override def simplify: NumberExpressionSelector

  /** Decides if an element's position is selected by the `a` cycle size and `b` offset value.
    *
    * @param position the position of the element amongst its siblings
    * @return `true` if the position is selected, `false` otherwise
    */
  protected def isElementPositionSelected(position: Int): Boolean = {
    if (a == 0) position == b else (position - b) % a == 0 && (position - b) / a >= 0
  }
}

/** Represents the CSS3 :nth-child structural pseudo-class selector. A :nth-child selector represents those elements in
  * the document tree that have a parent element, and have ''a''n + ''b'' - 1 siblings before them in the document tree,
  * for n ≥ 0. For values of ''a'' and ''b'' greater than zero, this effectively divides an element's children into
  * groups of ''a'' elements (the last group taking the remainder), and selects the ''b''th element of each group. If
  * both ''a'' and ''b'' are equal to zero, this selector represents no element in the document tree. The value ''a''
  * can be negative, but only the positive values of ''a''n + ''b'' may represent an element in the document tree. The
  * index of the first child of an element is 1.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <oddElem1 />
  *               <evenElem1 />
  *               <oddElem2 />
  *               <evenElem2 />
  *             </root>
  *   val selected = NthChildSelector(2, 1).select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<oddElem1 /><oddElem2 />` because both are
  * child elements, and for n = 0 the result of the ''2''n + ''1'' formula is 1, which selects the first child of the
  * `<root>` element, and for n = 1 the result of the ''2''n + ''1'' formula is 3, which selects the third child of the
  * `<root>` element.
  *
  * @constructor creates a new :nth-child selector with the specified ''a'' coefficient and ''b'' constant
  * @param a represents a cycle size
  * @param b represents an offset value
  */
case class NthChildSelector(a: Int, b: Int) extends NumberExpressionSelector {

  /** @inheritdoc */
  override def simplify: NthChildSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    nodeEnv.ancestors.nonEmpty && isElementPositionSelected(nodeEnv.before.size + 1)
  }
}

/** Represents the CSS3 :nth-last-child structural pseudo-class selector. A :nth-last-child selector represents those
  * elements in the document tree that have a parent element, and have ''a''n + ''b'' - 1 siblings after them in the
  * document tree, for n ≥ 0. For values of ''a'' and ''b'' greater than zero, this effectively divides an element's
  * children into groups of ''a'' elements (the first group taking the remainder), and selects the ''b''th last element
  * of each group. If both ''a'' and ''b'' are equal to zero, this selector represents no element in the document tree.
  * The value ''a'' can be negative, but only the positive values of ''a''n + ''b'' may represent an element in the
  * document tree. The index of the first child of an element is 1.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <oddElem1 />
  *               <evenElem1 />
  *               <oddElem2 />
  *               <evenElem2 />
  *             </root>
  *   val selected = NthLastChildSelector(2, 1).select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<evenElem1 /><evenElem2 />` because both are
  * child elements, and for n = 0 the result of the ''2''n + ''1'' formula is 1, which selects the first from the end
  * child of the `<root>` element, and for n = 1 the result of the ''2''n + ''1'' formula is 3, which selects the third
  * from the end child of the `<root>` element.
  *
  * @constructor creates a new :nth-last-child selector with the specified ''a'' coefficient and ''b'' constant
  * @param a represents a cycle size
  * @param b represents an offset value
  */
case class NthLastChildSelector(a: Int, b: Int) extends NumberExpressionSelector {

  /** @inheritdoc */
  override def simplify: NthLastChildSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    nodeEnv.ancestors.nonEmpty && isElementPositionSelected(nodeEnv.after.size + 1)
  }
}

/** Represents the CSS3 :nth-of-type structural pseudo-class selector. A :nth-of-type selector represents those elements
  * in the document tree that have a parent element, and have ''a''n + ''b'' - 1 siblings with the same expanded element
  * name before them in the document tree, for n ≥ 0. For values of ''a'' and ''b'' greater than zero, this effectively
  * divides the element's children into groups of ''a'' elements (the last group taking the remainder) per element type,
  * and selects the ''b''th element of each group. If both ''a'' and ''b'' are equal to zero, this selector represents
  * no element in the document tree. The value ''a'' can be negative, but only the positive values of ''a''n + ''b'' may
  * represent an element in the document tree. The index of the first child of an element for each element type group is
  * 1.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elemType1 idx="1" />
  *               <elemType2 idx="1" />
  *               <elemType1 idx="2" />
  *               <elemType2 idx="2" />
  *             </root>
  *   val selected = NthOfTypeSelector(2, 1).select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elemType1 idx="1" /><elemType2 idx="1" />`
  * because both are child elements and the children of the `<root>` element can be grouped into 2 element type groups -
  * `elemType1` and `elemType2` -, and for n = 0 the result of the ''2''n + ''1'' formula is 1, which selects the first
  * sibling of each element type group.
  *
  * @constructor creates a new :nth-of-type selector with the specified ''a'' coefficient and ''b'' constant
  * @param a represents a cycle size
  * @param b represents an offset value
  */
case class NthOfTypeSelector(a: Int, b: Int) extends NumberExpressionSelector {

  /** @inheritdoc */
  override def simplify: NthOfTypeSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    NthChildSelector(a, b).isSelected(
      nodeEnv.copy(
        before = nodeEnv.before.filter { sibling =>
          configuration.matches(
            sibling,
            if (nodeEnv.node.namespace == null) NoNamespace else UriNamespace(nodeEnv.node.namespace),
            Some(nodeEnv.node.label)
          )
        }
      )
    )(configuration)
  }
}

/** Represents the CSS3 :nth-last-of-type structural pseudo-class selector. A :nth-last-of-type selector represents
  * those elements in the document tree that have a parent element, and have ''a''n + ''b'' - 1 siblings with the same
  * expanded element name after them in the document tree, for n ≥ 0. For values of ''a'' and ''b'' greater than zero,
  * this effectively divides an element's children into groups of ''a'' elements (the first group taking the remainder)
  * per element type, and selects the ''b''th last element of each group. If both ''a'' and ''b'' are equal to zero,
  * this selector represents no element in the document tree. The value ''a'' can be negative, but only the positive
  * values of ''a''n + ''b'' may represent an element in the document tree. The index of the first child of an element
  * for each element type group is 1.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elemType1 idx="1" />
  *               <elemType2 idx="1" />
  *               <elemType1 idx="2" />
  *               <elemType2 idx="2" />
  *             </root>
  *   val selected = NthLastOfTypeSelector(2, 1).select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elemType1 idx="2" /><elemType2 idx="2" />`
  * because both are child elements and the children of the `<root>` element can be grouped into 2 element type groups -
  * `elemType1` and `elemType2` -, and for n = 0 the result of the ''2''n + ''1'' formula is 1, which selects the second
  * (the first from the end) sibling of each element type group.
  *
  * @constructor creates a :new nth-last-of-type selector with the specified ''a'' coefficient and ''b'' constant
  * @param a represents a cycle size
  * @param b represents an offset value
  */
case class NthLastOfTypeSelector(a: Int, b: Int) extends NumberExpressionSelector {

  /** @inheritdoc */
  override def simplify: NthLastOfTypeSelector = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    NthLastChildSelector(a, b).isSelected(
      nodeEnv.copy(
        after = nodeEnv.after.filter { sibling =>
          configuration.matches(
            sibling,
            if (nodeEnv.node.namespace == null) NoNamespace else UriNamespace(nodeEnv.node.namespace),
            Some(nodeEnv.node.label)
          )
        }
      )
    )(configuration)
  }
}

/** Represents the CSS3 :lang pseudo-class selector. A :lang(L) selector represents those elements in the document tree
  * that are in language ''L''. Whether an element is represented by a :lang(L) selector is based solely on the
  * element's language value being equal to the identifier ''L'', or beginning with the identifier ''L'' immediately
  * followed by "-" (U+002D). The identifier ''L'' must not be empty but it does not have to be a valid language name.
  *
  * The difference between the [[sandrasi.cssselectors.selectors.LangSelector LangSelector]] and the
  * [[sandrasi.cssselectors.selectors.DashAttributeSelector DashAttributeSelector]] is that the latter one only checks
  * the given attribute on the element itself, while the former one also checks the element's ancestors to perform the
  * selection.
  *
  * ==Language selector and namespaces==
  *
  * The language selector take a `namespace` argument. If `namespace` is
  * [[sandrasi.cssselectors.util.NoNamespace NoNamespace]], this language selector represents those elements that have
  * the specified language-attribute that is not in any namespace with the specified language. If `namespace` is
  * [[sandrasi.cssselectors.util.AnyNamespace AnyNamespace]], this language selector represents all elements that have
  * the specified language-attribute in any namespace with the specified language. Otherwise, if namespace is
  * [[sandrasi.cssselectors.util.UriNamespace UriNamespace]], it represents those elements that have the specified
  * language-attribute in the specified namespace with the specified language. If the namespace is invalid, this
  * language selector represents no elements.
  *
  * ==Usage==
  *
  * {{{
  *   val xml = <root>
  *               <elem1 lang="en">
  *                 <elem2 />
  *               <elem1 />
  *             </root>
  *   val selected = LangSelector("en").select(xml)
  * }}}
  *
  * In this example the value of the `selected` variable is going to be `<elem1 lang="en"><elem2 /></elem1 /><elem2 />`
  * because both `elem1` and `elem2` are in language ''en''.
  *
  * @constructor creates a language selector with the specified namespace, language-attribute name, and language
  * @param namespaceMatcher the namespace this language selector matches elements in
  * @param attrName the name of the language-attribute this language selector matches on
  * @param lang the language
  * @throws IllegalArgumentException if either `namespace`, or `attrName`, or `lang` is `null`, or if `lang` is empty
  */
case class LangSelector(namespace: Namespace, attrName: String, lang: String) extends FunctionalPseudoClassSelector {

  require(namespace != null, "namespace is required")
  require(attrName != null, "attrName is required")
  require(lang != null, "lang is required")
  require(lang.nonEmpty, "lang must not be empty")

  /** @inheritdoc */
  override def simplify: FunctionalPseudoClassSelector = if (namespace != InvalidNamespace) this else InvalidSelector

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    val attrSelector = DashAttributeSelector(namespace, attrName, lang)

    @tailrec def isSelected(nodeEnv: NodeEnvironment): Boolean = {
      val isNodeSelected = attrSelector.isSelected(nodeEnv)(configuration)
      if (isNodeSelected) {
        true
      } else if (nodeEnv.ancestors.isEmpty) {
        isNodeSelected
      } else {
        isSelected(nodeEnv.copy(node = nodeEnv.ancestors.head, ancestors = nodeEnv.ancestors.tail))
      }
    }

    isSelected(nodeEnv)
  }
}
/** Factory for [[selectors.LangSelector LangSelector]] instances. */
case object LangSelector {

  /** Creates a new language selector that matches on the `lang` attribute with the specified language in all
    * namespaces.
    */
  def apply(lang: String): LangSelector = LangSelector(AnyNamespace, "lang", lang)

  /** Creates a new language selector that matches on the `lang` attribute with the specified language in the specified
    * namespace. If `namespaceUri` is `null` the language selector will match on those `lang` attributes that are not in
    * any namespace.
    */
  def apply(namespaceUri: String, lang: String): LangSelector = {
    LangSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), "lang", lang)
  }

  /** Creates a new language selector that matches on the specified attribute with the specified language in the
    * specified namespace. If `namespaceUri` is `null` the language selector will match on those specified attributes
    * with the specified language that are not in any namespace.
    */
  def apply(namespaceUri: String, attrName: String, lang: String): LangSelector = {
    LangSelector(if (namespaceUri == null) NoNamespace else UriNamespace(namespaceUri), attrName, lang)
  }
}

/** A common base class for selectors that permit element selection based on abstractions about the document tree beyond
  * those specified by the document language.
  */
sealed trait PseudoElementSelector extends SelectorSequence {
  override def simplify: PseudoElementSelector
}

case class FirstLineSelector() extends PseudoElementSelector {
  override def simplify: PseudoElementSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class FirstLetterSelector() extends PseudoElementSelector {
  override def simplify: PseudoElementSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class BeforeSelector() extends PseudoElementSelector {
  override def simplify: PseudoElementSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

case class AfterSelector() extends PseudoElementSelector {
  override def simplify: PseudoElementSelector = ???
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = ???
}

/** A common base class for selectors that perform negation. */
sealed trait NegationSelector extends ChainableSelector {

  /** @inheritdoc */
  override def simplify: NegationSelector
}

/** Represents the CSS3 :not pseudo-class selector. A :not(S) selector represents those elements in the document tree
  * that are not selected by the simple selector ''S''. A simple selector is either a type selector, a universal
  * selector, an attribute selector, a class selector, an ID selector, or a pseudo-class selector (excluding the
  * negation selector itself).
  *
  * Note: the negation selector allows useless selectors to be written. For instance
  * `NotSelector(UniversalSelector(Some(AnyNamespaceMatcher)))`, which represents no element at all, or
  * `SimpleSelectorSequenceSelector(Some(TypeSelector("elem1")), NotSelector(TypeSelector("elem2")))`, which is
  * equivalent to `TypeSelector("elem1")`.
  *
  * @constructor creates a negation selector with the specified attribute
  * @param argument a simple selector
  * @throws IllegalArgumentException if `argument` is `null`
  */
case class NotSelector(argument: NegatableSelector) extends NegationSelector {

  require(argument != null, "argument is required")

  /** @inheritdoc */
  override def simplify: NegationSelector = argument.simplify match {
    case InvalidSelector => InvalidSelector
    case simplifiedArgument @ _ => NotSelector(simplifiedArgument)
  }

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = {
    !argument.isSelected(nodeEnv)(configuration)
  }
}

/** Represents an invalid CSS3 selector. An invalid selector represents no element in the document tree. */
case object InvalidSelector extends SelectorGroup
  with SelectorCombination with SelectorSequence with ElementSelector with BaseAttributeSelector with PseudoClassSelector
  with FunctionalPseudoClassSelector with PseudoElementSelector with NegationSelector {

  /** @inheritdoc */
  override def simplify: InvalidSelector.type = this

  /** @inheritdoc */
  override protected[selectors] def isSelected(nodeEnv: NodeEnvironment)(configuration: Configuration): Boolean = false
}
