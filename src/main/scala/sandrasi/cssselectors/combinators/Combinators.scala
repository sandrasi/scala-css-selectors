package sandrasi.cssselectors.combinators

sealed trait Combinator

case class DescendantCombinator() extends Combinator

case class ChildCombinator() extends Combinator

case class AdjacentSiblingCombinator() extends Combinator

case class GeneralSiblingCombinator() extends Combinator
