package sandrasi.cssselectors

import sandrasi.cssselectors.combinators.Combinator

import scala.language.implicitConversions
import scala.xml.NodeSeq

object OldSelectors {

//  implicit class CssSelectorsSupport(ns: NodeSeq) {
//
//    def \\\(selector: String): NodeSeq = new SelectorParser().parse(selector).select(ns)
//  }

  sealed trait NodeSeqSelector {

    def select(ns: NodeSeq): NodeSeq
    def simplify: NodeSeqSelector
  }

  sealed trait NegatableSelector extends NodeSeqSelector

  sealed trait ElementSelector extends NodeSeqSelector with NegatableSelector {

    override def simplify: NodeSeqSelector = this
  }

  case class UniversalSelector(namespace: Option[NamespacePrefix]) extends ElementSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class TypeSelector(namespace: Option[NamespacePrefix], name: String) extends ElementSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  sealed trait SelectorSequenceMember extends NodeSeqSelector {

    override def simplify: NodeSeqSelector = this
  }

  case class AttributeSelector(namespace: Option[NamespacePrefix], name: String, matcher: Option[AttributeValueMatcher]) extends SelectorSequenceMember with NegatableSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class ClassSelector(name: String) extends SelectorSequenceMember with NegatableSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class IdSelector(name: String) extends SelectorSequenceMember with NegatableSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  sealed trait PseudoSelector extends SelectorSequenceMember

  sealed trait PseudoClassSelector extends PseudoSelector with NegatableSelector

  case class RootSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class FirstChildSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class LastChildSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class FirstOfTypeSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class LastOfTypeSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class OnlyChildSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class OnlyOfTypeSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class EmptySelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class LinkSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class VisitedSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class ActiveSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class HoverSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class FocusSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class TargetSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class EnabledSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class DisabledSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class CheckedSelector() extends PseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class NumberExpression(a: Int, b: Int)

  sealed trait FunctionalPseudoClassSelector extends PseudoClassSelector

  case class NthChildSelector(argument: NumberExpression) extends FunctionalPseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class NthLastChildSelector(argument: NumberExpression) extends FunctionalPseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class NthOfTypeSelector(argument: NumberExpression) extends FunctionalPseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class NthLastOfTypeSelector(argument: NumberExpression) extends FunctionalPseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class LangSelector(argument: String) extends FunctionalPseudoClassSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  sealed trait PseudoElementSelector extends PseudoSelector

  case class FirstLineSelector() extends PseudoElementSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class FirstLetterSelector() extends PseudoElementSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class BeforeSelector() extends PseudoElementSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  case class AfterSelector() extends PseudoElementSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
  }

  sealed trait NegationSelector extends SelectorSequenceMember

  case class NotSelector(argument: NegatableSelector) extends NegationSelector {

    override def select(ns: NodeSeq): NodeSeq = ???

    override def simplify: NodeSeqSelector = (argument.simplify: @unchecked) match {
      case InvalidSelector => InvalidSelector
      case simplifiedArgument: NegatableSelector => NotSelector(simplifiedArgument)
    }
  }

  sealed trait SimpleSelector extends NodeSeqSelector

  case class SimpleSelectorSequence(elementSelector: Option[ElementSelector], selectorSequenceMembers: List[SelectorSequenceMember]) extends SimpleSelector {

    require(elementSelector.isDefined || selectorSequenceMembers.nonEmpty, "SimpleSelectorSequence may not be empty")

    override def select(ns: NodeSeq): NodeSeq = ???

    override def simplify: NodeSeqSelector = (elementSelector.map(_.simplify), selectorSequenceMembers) match {
      case (Some(es), Nil) => es
      case (None, ssm :: Nil) => ssm.simplify
      case (None, ssm :: tail) => ???
      case (Some(es), ssm :: tail) => ???
    }

    private def simplify(selectorSequenceMembers: List[SelectorSequenceMember]) = selectorSequenceMembers
  }

  sealed trait Selector extends NodeSeqSelector

  case class SelectorCombination(simpleSelectorSequence: SimpleSelector, combinedSelector: Option[CombinedSelector]) extends Selector {

    override def select(ns: NodeSeq): NodeSeq = ???
    override def simplify: NodeSeqSelector = ???
  }

  case class CombinedSelector(combinator: Combinator, selectorCombination: SelectorCombination) extends NodeSeqSelector {

    override def select(ns: NodeSeq): NodeSeq = ???
    override def simplify: NodeSeqSelector = ???
  }

  sealed trait SelectorsGroup extends NodeSeqSelector

  case class GroupSelector(selectors: List[Selector]) extends SelectorsGroup {

    override def select(ns: NodeSeq): NodeSeq = ???
    override def simplify: NodeSeqSelector = ???
  }

  case object InvalidSelector extends NodeSeqSelector with PseudoClassSelector with FunctionalPseudoClassSelector
                              with PseudoElementSelector with NegationSelector with SimpleSelector
                              with Selector with SelectorsGroup {

    override def select(ns: NodeSeq): NodeSeq = NodeSeq.Empty

    override def simplify: NodeSeqSelector = this
  }

//  private def selectByAttribute(ns: NodeSeq, name: String, matcher: Option[AttributeValueMatcher])(converter: (String) => String) = (ns \\ "_").view.filter { n =>
//     n.attributes.find(attr => converter(attr.key) == converter(name)).flatMap(attr => attr.get(attr.key)) match {
//       case Some(Text(value)) => matcher.map(_.matches(value)(converter)).getOrElse(true)
//       case _ => false
//     }
//  }
//
//  @tailrec private def isValidSelectorSequence(selectorSequenceMembers: List[SelectorSequenceMember]): Boolean = selectorSequenceMembers match {
//     case Nil => true
//     case InvalidSelector :: _ => false
//     case FirstLineSelector() :: tail if !tail.isEmpty => false
//     case FirstLetterSelector() :: tail if !tail.isEmpty => false
//     case BeforeSelector() :: tail if !tail.isEmpty => false
//     case AfterSelector() :: tail if !tail.isEmpty => false
//     case _ => isValidSelectorSequence(selectorSequenceMembers.tail)
//  }
}
