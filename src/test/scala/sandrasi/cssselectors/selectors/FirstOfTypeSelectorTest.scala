package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

class FirstOfTypeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/first_of_type_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = FirstOfTypeSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the first siblings of their type in the list of child elements") {
      FirstOfTypeSelector().select(testXml) should be(trim(<elem1 idx="1" /><elem2 idx="1" />))
    }
  }
}
