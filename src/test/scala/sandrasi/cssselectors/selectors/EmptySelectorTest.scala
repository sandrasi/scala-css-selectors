package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

import scala.xml.{PCData, Unparsed}

class EmptySelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/empty_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = EmptySelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the empty elements") {
      EmptySelector().select(testXml) should be(trim(<elem1 /><elem2 /><elem3 /><elem4 /><elem5><?processing-instruction?></elem5>))
    }

    it("selects elements containing only comment") {
      EmptySelector().select(<root><!-- comment --></root>) should be(trim(<root><!-- comment --></root>))
    }

    it("selects elements containing only processing instructions") {
      EmptySelector().select(<root><?processing-instruction?></root>) should be(trim(<root><?processing-instruction?></root>))
    }

    it("selects elements containing only empty PCDATA section") {
      EmptySelector().select(<root>{PCData("")}</root>) should be(trim(<root>{PCData("")}</root>))
    }

    it("selects elements containing only empty unparsed text") {
      EmptySelector().select(<root>{Unparsed("")}</root>) should be(trim(<root>{Unparsed("")}</root>))
    }
  }
}
