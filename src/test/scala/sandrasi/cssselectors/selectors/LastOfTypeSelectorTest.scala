package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

class LastOfTypeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/last_of_type_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = LastOfTypeSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the last siblings of their type in the list of child elements") {
      LastOfTypeSelector().select(testXml) should be(trim(<elem1 idx="2" /><elem2 idx="2" />))
    }
  }
}
