package sandrasi.cssselectors

import sandrasi.cssselectors.configuration.{Configuration, DefaultConfiguration}
import sandrasi.cssselectors.grammar.SelectorParser

import scala.xml.NodeSeq

// TODO (sandrasi): test it
/** Provides an implicit class that adds a method to the [[scala.xml.NodeSeq NodeSeq]] class. */
object CssSelectorsSupport {

  /** Wraps an instance of the [[scala.xml.NodeSeq NodeSeq]] class and provides the `\\\` method on it to conveniently
    * select document elements using CSS3 selector patterns.
    *
    * @constructor creates a new `CssSelectorsEnhancedNodeSeq` with the specified node sequence
    * @param ns the node sequence from which elements can be selected using the `\\\` method
    */
  implicit class CssSelectorsEnhancedNodeSeq(ns: NodeSeq) {

    /** Selects those nodes of the `ns` [[scala.xml.NodeSeq NodeSeq]] that match the `selectorPattern` string using the
      * given configuration. `\\\` can be used in combination with the `\` and `\\` methods provided by the Scala XML
      * library.
      *
      * ==Usage==
      *
      * {{{
      *   import scala.xml._
      *   import sandrasi.cssselectors.CssSelectorsSupport._
      *
      *   val xml = XML.loadFile("example.xhtml")
      *
      *   val hyperlinks = xml \\\ "a[href='http://www.example.com']"
      * }}}
      *
      * In the above example the value of `hyperlinks` is a sequence of ''a'' elements that have an ''href'' attribute
      * with value ''http://www.example.com''.
      *
      * @param selectorPattern a CSS selector pattern that matches against elements in a document tree
      * @param configuration a [[sandrasi.cssselectors.configuration.Configuration configuration]] for node selection
      * @return a sequence of selected `Node`s. If `selectorPattern` is invalid, an empty `NodeSeq` is returned.
      */
    def \\\(selectorPattern: String)(implicit configuration: Configuration = DefaultConfiguration): NodeSeq = {
      new SelectorParser().parse(selectorPattern).select(ns)
    }
  }
}
