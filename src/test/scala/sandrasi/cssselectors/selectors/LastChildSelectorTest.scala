package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

class LastChildSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/last_child_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = LastChildSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the last child elements") {
      LastChildSelector().select(testXml) should be(trim(<elem idx="2" />))
    }
  }
}
