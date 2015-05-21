package sandrasi.cssselectors.grammar

import sandrasi.cssselectors.combinators._
import sandrasi.cssselectors.lexicalscanners.SelectorTokenizer._
import sandrasi.cssselectors.selectors._
import sandrasi.cssselectors.util._

import scala.util.parsing.combinator.RegexParsers

// TODO (sandrasi): documentation
class SelectorParser(defaultNamespace: Option[String] = None, namespaces: Map[String, String] = Map.empty) extends RegexParsers {

  require(defaultNamespace != null, "defaultNamespace is required")
  require(namespaces != null, "namespaces is required")

  def parse(selectorPattern: String): NodeSeqSelector = parseAll(selectorsGroup, selectorPattern).getOrElse(InvalidSelector).simplify

  override def skipWhitespace: Boolean = false

  private def selectorsGroup: Parser[SelectorGroup] = rep1sep(selector, COMMA ~ S.*) ^^ { toSelectorGroup }
  private def selector: Parser[SelectorCombination] = simpleSelectorSequence ~ (combinator ~ simpleSelectorSequence).* ^^ { toSelector }
  private def simpleSelectorSequence: Parser[SelectorSequence] = elementSelector.? ~ chainableSelector.* ~ pseudoElement.? ^^ { toSelectorSequence }
  private def elementSelector: Parser[ElementSelector] = typeSelector | universal
  private def chainableSelector: Parser[ChainableSelector] = id | `class` | attribute | negation | pseudoClass
  private def combinator: Parser[Combinator] = (PLUS <~ S.* | GREATER <~ S.* | TILDE <~ S.* | S <~ S.*) ^^ { toCombinator }
  private def typeSelector: Parser[TypeSelector] = namespacePrefix.? ~ elementName ^^ { toTypeSelector }
  private def namespacePrefix: Parser[Namespace] = (IDENT ^^ { decode } | "*").? <~ "|" ^^ { toNamespace }
  private def elementName: Parser[String] = IDENT ^^ { decode }
  private def universal: Parser[UniversalSelector] = namespacePrefix.? <~ "*" ^^ { toUniversalSelector }
  private def id: Parser[IdSelector] = HASH ^^ { hash => IdSelector(decode(hash.drop(1))) }
  private def `class`: Parser[ClassSelector] = "." ~> IDENT ^^ { ident => ClassSelector(decode(ident)) }
  private def attribute: Parser[AttributeSelector] = "[" ~> attributeName ~ (attributeValueMatcher ~ attributeValue).? <~ "]" ^^ { toAttributeSelector }
  private def attributeName = S.* ~> (namespacePrefix.? ~ IDENT) <~ S.* ^^ { case ~(namespace, ident) => new ~(namespace, decode(ident)) }
  private def attributeValueMatcher: Parser[String] = PREFIX_MATCH | SUFFIX_MATCH | SUBSTRING_MATCH | "=" | INCLUDES | DASH_MATCH
  private def attributeValue: Parser[String] = S.* ~> (IDENT ^^ { decode } | STRING ^^ { string => decode(string.drop(1).dropRight(1)) }) <~ S.*
  private def pseudoClass: Parser[PseudoClassSelector] = ":" ~> (functionalPseudoClass | IDENT ^^ { ident => toPseudoClassSelector(decode(ident)) })
  private def functionalPseudoClass: Parser[FunctionalPseudoClassSelector] = ((FUNCTION ^^ { decode }) <~ S.*) ~ expression <~ ")" ^^ { toFunctionalPseudoClassSelector }
  private def pseudoElement: Parser[PseudoElementSelector] = "::" ~> IDENT ^^ { ident => toPseudoElementSelector(decode(ident)) }
  private def expression: Parser[String] = (term ~ S.*).+ ^^ { toExpression }
  private def term: Parser[String] = PLUS | "-" | DIMENSION ^^ { decode } | NUMBER | STRING ^^ { dropQuotes } | IDENT ^^ { decode }
  private def negation: Parser[NegationSelector] = ":" ~ NOT ~ "(" ~ S.* ~> negationArg <~ S.* ~ ")" ^^ { toNegationSelector }
  private def negationArg: Parser[NegatableSelector] = elementSelector | id | `class` | attribute | pseudoClass

  private def toSelectorGroup(selectors: List[SelectorCombination]) = SelectorsGroupSelector(selectors.head, selectors.tail:_*)

  private def toSelector(selectorCombination: ~[SelectorSequence, List[~[Combinator, SelectorSequence]]]) = {
    selectorCombination match {
      case ~(simpleSelector, combinedSelectorSequences) => Selector(
        simpleSelector,
        combinedSelectorSequences.map { case ~(combinator, selectorSequence) => (combinator, selectorSequence) }:_*
      )
    }
  }

  private def toSelectorSequence(simpleSelector: ~[~[Option[ElementSelector], List[ChainableSelector]], Option[PseudoElementSelector]]) = {
    simpleSelector match {
      case ~(~(elementSelector, chainableSelectors), pseudoElementSelector) =>
        SimpleSelectorSequenceSelector(elementSelector, chainableSelectors:_*)(pseudoElementSelector)
    }
  }

  private def toCombinator(combinator: String) = combinator match {
    case "+" => AdjacentSiblingCombinator()
    case ">" => ChildCombinator()
    case "~" => GeneralSiblingCombinator()
    case "" => DescendantCombinator()
    case unknownCombinator => throw new IllegalArgumentException(s"Unknown combinator '$unknownCombinator'")
  }

  private def toTypeSelector(typeSelector: ~[Option[Namespace], String]) = typeSelector match {
    case ~(namespace, elementName) => TypeSelector(withDefaultNamespace(namespace), elementName)
  }

  private def toUniversalSelector(namespace: Option[Namespace]) = UniversalSelector(withDefaultNamespace(namespace))

  private def withDefaultNamespace(namespace: Option[Namespace]) = namespace.getOrElse(defaultNamespace.map(UriNamespace).getOrElse(AnyNamespace))

  private def toNamespace(namespacePrefix: Option[String]) = namespacePrefix.map {
    case "*" => AnyNamespace
    case prefix if namespaces.contains(prefix) => UriNamespace(namespaces(prefix))
    case _ => InvalidNamespace
  }.getOrElse(NoNamespace)

  private def toAttributeSelector(attribute: ~[~[Option[Namespace], String], Option[~[String, String]]]) = attribute match {
    case ~(~(ns, attrName), Some(~(matcher, attrValue))) => matcher match {
      case PREFIX_MATCH => PrefixAttributeSelector(ns.getOrElse(AnyNamespace), attrName, attrValue)
      case SUFFIX_MATCH => SuffixAttributeSelector(ns.getOrElse(AnyNamespace), attrName, attrValue)
      case SUBSTRING_MATCH => SubstringAttributeSelector(ns.getOrElse(AnyNamespace), attrName, attrValue)
      case "=" => ExactAttributeSelector(ns.getOrElse(AnyNamespace), attrName, attrValue)
      case INCLUDES => IncludesAttributeSelector(ns.getOrElse(AnyNamespace), attrName, attrValue)
      case DASH_MATCH => DashAttributeSelector(ns.getOrElse(AnyNamespace), attrName, attrValue)
      case unknownMatcher => throw new NotImplementedError(s"Unknown attribute matcher '$unknownMatcher'")
    }
    case ~(~(ns, attrName), None) => AttributePresenceSelector(ns.getOrElse(AnyNamespace), attrName)
  }

  private def toPseudoClassSelector(pseudoClass: String) = pseudoClass.toLowerCase match {
    case "root" => RootSelector()
    case "first-child" => FirstChildSelector()
    case "last-child" => LastChildSelector()
    case "first-of-type" => FirstOfTypeSelector()
    case "last-of-type" => LastOfTypeSelector()
    case "only-child" => OnlyChildSelector()
    case "only-of-type" => OnlyOfTypeSelector()
    case "empty" => EmptySelector()
    case "link" => LinkSelector()
    case "visited" => VisitedSelector()
    case "active" => ActiveSelector()
    case "hover" => HoverSelector()
    case "focus" => FocusSelector()
    case "target" => TargetSelector()
    case "enabled" => EnabledSelector()
    case "disabled" => DisabledSelector()
    case "checked" => CheckedSelector()
    case _ => InvalidSelector
  }

  private def toFunctionalPseudoClassSelector(pseudoClass: ~[String, String]): FunctionalPseudoClassSelector = pseudoClass match {
    case ~(name: String, arg: String) => name.dropRight(1).toLowerCase match {
      case "nth-child" => parseAll(nth, arg).map { case (a, b) => NthChildSelector(a, b) }.getOrElse(InvalidSelector)
      case "nth-last-child" => parseAll(nth, arg).map { case (a, b) => NthLastChildSelector(a, b) }.getOrElse(InvalidSelector)
      case "nth-of-type" => parseAll(nth, arg).map { case (a, b) => NthOfTypeSelector(a, b) }.getOrElse(InvalidSelector)
      case "nth-last-of-type" => parseAll(nth, arg).map { case (a, b) => NthLastOfTypeSelector(a, b) }.getOrElse(InvalidSelector)
      case "lang" => LangSelector(arg)
      case _ => InvalidSelector
    }
  }

  private def nth: Parser[(Int, Int)] = S.* ~> (fullNth | partialNth | odd | even) <~ S.*
  private def fullNth: Parser[(Int, Int)] = ("-" | PLUS).? ~ INTEGER.? ~ N ~ (S.* ~> ("-" | PLUS) ~ (S.* ~> INTEGER)).? ^^ { toFullNumberExpression }
  private def partialNth: Parser[(Int, Int)] = ("-" | PLUS).? ~ INTEGER ^^ { toPartialNumberExpression }
  private def odd: Parser[(Int, Int)] = ODD ^^ { odd => (2, 1) }
  private def even: Parser[(Int, Int)] = EVEN ^^ { even => (2, 0) }

  private def toFullNumberExpression(numberExpression: ~[~[~[Option[String], Option[String]], String], Option[~[String, String]]]) = numberExpression match {
    case ~(~(~(Some(aSign), Some(a)), _), Some(~(bSign, b))) => (if (aSign == "-") -a.toInt else a.toInt, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(Some(aSign), Some(a)), _), None) => (if (aSign == "-") -a.toInt else a.toInt, 0)
    case ~(~(~(Some(aSign), None), _), Some(~(bSign, b))) => (if (aSign == "-") -1 else 1, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(Some(aSign), None), _), None) => (if (aSign == "-") -1 else 1, 0)
    case ~(~(~(None, Some(a)), _), Some(~(bSign, b))) => (a.toInt, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(None, Some(a)), _), None) => (a.toInt, 0)
    case ~(~(~(None, None), _), Some(~(bSign, b))) => (1, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(None, None), _), None) => (1, 0)
  }

  private def toPartialNumberExpression(numberExpression: ~[Option[String], String]) = numberExpression match {
    case ~(Some(bSign), b) => (0, if (bSign == "-") -b.toInt else b.toInt)
    case ~(None, b) => (0, b.toInt)
  }

  private def toPseudoElementSelector(pseudoElement: String) = pseudoElement.toLowerCase match {
    case "first-line" => FirstLineSelector()
    case "first-letter" => FirstLetterSelector()
    case "before" => BeforeSelector()
    case "after" => AfterSelector()
    case _ => InvalidSelector
  }

  private def dropQuotes(string: String) = decode(string.drop(1).dropRight(1))

  private def toExpression(expression: List[~[String, List[String]]]) = {
    expression.map { case ~(term, whitespace) => term + whitespace.mkString }.mkString
  }

  private def toNegationSelector(negatableSelector: NegatableSelector) = NotSelector(negatableSelector)
}
