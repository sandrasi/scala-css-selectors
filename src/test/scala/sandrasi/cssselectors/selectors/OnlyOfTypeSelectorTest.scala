package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

class OnlyOfTypeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/only_of_type_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = OnlyOfTypeSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the child elements that have no siblings with the same expanded element name") {
      OnlyOfTypeSelector().select(testXml) should be(trim(<elem1 /><elem2 />))
    }
  }
}
