package sandrasi.cssselectors.selectors

import scala.xml.{NodeSeq, Utility, XML}

trait Fixture {

  protected def fixtureFile: String

  protected lazy val testXml: NodeSeq = trim(XML.load(getClass.getResource(fixtureFile)))

  protected def trim(ns: NodeSeq) = NodeSeq.fromSeq(ns.map(Utility.trimProper).flatten)
}
