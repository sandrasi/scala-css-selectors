package sandrasi.cssselectors.grammar

import java.util.regex.Matcher

import sandrasi.cssselectors.{AttributeValueMatcher, NamespacePrefix}
import sandrasi.cssselectors.OldSelectors._
import sandrasi.cssselectors.combinators._
import sandrasi.cssselectors.lexicalscanners.SelectorTokenizer._

import scala.util.parsing.combinator.RegexParsers

@deprecated
class OldSelectorParser() extends RegexParsers {

  def parse(selectorPattern: String): NodeSeqSelector = parseAll(selectorsGroup, selectorPattern).getOrElse(InvalidSelector).simplify

  override def skipWhitespace: Boolean = false

  private def selectorsGroup: Parser[SelectorsGroup] = rep1sep(selector, COMMA ~ S.*) ^^ { toSelectorsGroup }
  private def selector: Parser[Selector] = simpleSelectorSequence ~ (combinator ~ simpleSelectorSequence).* ^^ { toSelector }
  private def simpleSelectorSequence: Parser[SimpleSelector] =
    (typeSelector | universal) ~ (HASH ^^ { h => IdSelector(decode(h.drop(1))) } | `class` | attrib | negation | pseudo).* ^^ { toSimpleSelector } |
    (HASH ^^ { h => IdSelector(decode(h.drop(1))) } | `class` | attrib | negation | pseudo).+ ^^ { toSimpleSelector }
  private def combinator: Parser[Combinator] = (PLUS <~ S.* | GREATER <~ S.* | TILDE <~ S.* | S <~ S.*) ^^ { toCombinator }
  private def typeSelector: Parser[TypeSelector] = namespacePrefix.? ~ elementName ^^ { case ~(namespacePrefix, elementName) => TypeSelector(namespacePrefix, elementName) }
  private def namespacePrefix: Parser[NamespacePrefix] = (IDENT ^^ { decode } | "*").? <~ "|" ^^ { prefix => NamespacePrefix(prefix) }
  private def elementName: Parser[String] = IDENT ^^ { decode }
  private def universal: Parser[UniversalSelector] = namespacePrefix.? <~ "*" ^^ { UniversalSelector }
  private def `class`: Parser[ClassSelector] = "." ~> IDENT ^^ { i => ClassSelector(decode(i)) }
  private def attrib: Parser[AttributeSelector] =
    "[" ~ S.* ~> ((namespacePrefix ~ IDENT ^^ { case ~(ns, i) => new ~(Some(ns), decode(i)) } | IDENT ^^ { i => new ~(None, decode(i)) }) <~ S.*) ~
    ((PREFIX_MATCH | SUFFIX_MATCH | SUBSTRING_MATCH | "=" | INCLUDES | DASH_MATCH) ~ (S.* ~> (IDENT ^^ { decode } | STRING ^^ { s => decode(s.drop(1).dropRight(1)) }) <~ S.*)).? <~ "]" ^^ { toAttributeSelector }
  private def pseudo: Parser[PseudoSelector] = pseudoClass | pseudoElement
  private def pseudoClass: Parser[PseudoClassSelector] = ":" ~> (functionalPseudoClass | IDENT ^^ { i => toPseudoClassSelector(decode(i)) })
  private def functionalPseudoClass: Parser[FunctionalPseudoClassSelector] = ((FUNCTION ^^ { decode }) <~ S.*) ~ expression <~ ")" ^^ { toFunctionalPseudoClassSelector }
  private def pseudoElement: Parser[PseudoElementSelector] = "::" ~> IDENT ^^ { i => toPseudoElementSelector(decode(i)) }
  private def expression: Parser[String] = ((PLUS | "-" | DIMENSION ^^ { decode } | NUMBER | STRING ^^ { s => decode(s.drop(1).dropRight(1)) } | IDENT ^^ { decode }) ~ S.*).+ ^^ { _.map { case ~(term, whitespace) => term + whitespace.mkString }.mkString }
  private def negation: Parser[NegationSelector] = NOT ~ S.* ~> negationArg <~ S.* ~ ")" ^^ { toNegationSelector }
  private def negationArg: Parser[NegatableSelector] = typeSelector | universal | HASH ^^ { h => IdSelector(h.drop(1)) } | `class` | attrib | pseudoClass
  private def nth: Parser[NumberExpression] = S.* ~> (
    ("-" | PLUS).? ~ INTEGER.? ~ N ~ (S.* ~> ("-" | PLUS) ~ (S.* ~> INTEGER)).? ^^ { toFullNumberExpression } |
    ("-" | PLUS).? ~ INTEGER ^^ { toPartialNumberExpression } |
    ODD ^^ { o => NumberExpression(2, 1) } |
    EVEN ^^ { e => NumberExpression(2, 0) }
  ) <~ S.*

  private def toSelectorsGroup(selectors: List[Selector]): SelectorsGroup = GroupSelector(selectors)

  // TODO (sandrasi): make this tail-recursive
  private def toSelector(combinedSelector: ~[SimpleSelector, List[~[Combinator, SimpleSelector]]]): Selector = combinedSelector match {
    case ~(simpleSelector, Nil) => SelectorCombination(simpleSelector, None)
    case ~(simpleSelector, ~(combinator, combinedSimpleSelector) :: tail) => toSelector(new ~(combinedSimpleSelector, tail)) match {
      case sc @ SelectorCombination(_, _) => SelectorCombination(simpleSelector, Some(CombinedSelector(combinator, sc)))
      case InvalidSelector => InvalidSelector
    }
  }

  private def toSimpleSelector(simpleSelector: ~[ElementSelector, List[SelectorSequenceMember]]): SimpleSelector = simpleSelector match {
    case ~(elementSelector, selectorSequenceMembers) => SimpleSelectorSequence(Some(elementSelector), selectorSequenceMembers)
  }

  private def toSimpleSelector(selectorSequenceMembers: List[SelectorSequenceMember]): SimpleSelector = SimpleSelectorSequence(None, selectorSequenceMembers)

  private def toCombinator(combinator: String): Combinator = combinator.trim() match {
    case "+" => AdjacentSiblingCombinator()
    case ">" => ChildCombinator()
    case "~" => GeneralSiblingCombinator()
    case "" => DescendantCombinator()
    case c @ _ => throw new IllegalArgumentException(s"Unknown combinator '$c'")
  }

  private def toAttributeSelector(attribute: ~[~[Option[NamespacePrefix], String], Option[~[String, String]]]): AttributeSelector = attribute match {
    case ~(~(namespace, attrName), Some(~(matcher, attrValue))) => matcher.trim match {
      case PREFIX_MATCH => AttributeSelector(namespace, attrName, Some(new AttributeValueMatcher(attrValue)))
      case SUFFIX_MATCH => AttributeSelector(namespace, attrName, Some(new AttributeValueMatcher(attrValue)))
      case SUBSTRING_MATCH => AttributeSelector(namespace, attrName, Some(new AttributeValueMatcher(attrValue)))
      case "=" => AttributeSelector(namespace, attrName, Some(new AttributeValueMatcher(attrValue)))
      case INCLUDES => AttributeSelector(namespace, attrName, Some(new AttributeValueMatcher(attrValue)))
      case DASH_MATCH => AttributeSelector(namespace, attrName, Some(new AttributeValueMatcher(attrValue)))
      case m @ _ => throw new NotImplementedError(s"Unknown attribute matcher '$m'")
    }
    case ~(~(namespace, attrName), None) => AttributeSelector(namespace, attrName, None)
  }

  private def toPseudoClassSelector(pseudoClass: String): PseudoClassSelector = pseudoClass.toLowerCase match {
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
      case "nth-child" => parseAll(nth, arg).map(NthChildSelector).getOrElse(InvalidSelector)
      case "nth-last-child" => parseAll(nth, arg).map(NthLastChildSelector).getOrElse(InvalidSelector)
      case "nth-of-type" => parseAll(nth, arg).map(NthOfTypeSelector).getOrElse(InvalidSelector)
      case "nth-last-of-type" => parseAll(nth, arg).map(NthLastOfTypeSelector).getOrElse(InvalidSelector)
      case "lang" => LangSelector(arg)
      case _ => InvalidSelector
    }
  }

  private def toPseudoElementSelector(pseudoElement: String): PseudoElementSelector = pseudoElement.toLowerCase match {
    case "first-line" => FirstLineSelector()
    case "first-letter" => FirstLetterSelector()
    case "before" => BeforeSelector()
    case "after" => AfterSelector()
    case _ => InvalidSelector
  }

  private def toNegationSelector(negatableSelector: NegatableSelector): NegationSelector = NotSelector(negatableSelector)

  private def toFullNumberExpression(fa: ~[~[~[Option[String], Option[String]], String], Option[~[String, String]]]): NumberExpression = fa match {
    case ~(~(~(Some(aSign), Some(a)), _), Some(~(bSign, b))) => NumberExpression(if (aSign == "-") -a.toInt else a.toInt, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(Some(aSign), Some(a)), _), None) => NumberExpression(if (aSign == "-") -a.toInt else a.toInt, 0)
    case ~(~(~(Some(aSign), None), _), Some(~(bSign, b))) => NumberExpression(if (aSign == "-") -1 else 1, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(Some(aSign), None), _), None) => NumberExpression(if (aSign == "-") -1 else 1, 0)
    case ~(~(~(None, Some(a)), _), Some(~(bSign, b))) => NumberExpression(a.toInt, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(None, Some(a)), _), None) => NumberExpression(a.toInt, 0)
    case ~(~(~(None, None), _), Some(~(bSign, b))) => NumberExpression(1, if (bSign == "-") -b.toInt else b.toInt)
    case ~(~(~(None, None), _), None) => NumberExpression(1, 0)
  }

  private def toPartialNumberExpression(fa: ~[Option[String], String]): NumberExpression = fa match {
    case ~(Some(bSign), b) => NumberExpression(0, if (bSign == "-") -b.toInt else b.toInt)
    case ~(None, b) => NumberExpression(0, b.toInt)
  }
}
