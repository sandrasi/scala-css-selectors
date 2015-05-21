package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

class OnlyChildSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/only_child_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = OnlyChildSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the child elements that have no siblings") {
      OnlyChildSelector().select(testXml) should be(trim(<elem1 />))
    }
  }
}
