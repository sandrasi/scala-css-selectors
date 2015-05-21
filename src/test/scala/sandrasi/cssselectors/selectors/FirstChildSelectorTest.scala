package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

class FirstChildSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/first_child_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = FirstChildSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the first child elements") {
      FirstChildSelector().select(testXml) should be(trim(<elem idx="1" />))
    }
  }
}
