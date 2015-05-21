package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

class RootSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/root_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = RootSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects the root element") {
      RootSelector().select(testXml) should be(trim(<root><elem /></root>))
    }

    it("selects the root elements") {
      RootSelector().select(trim(<root1 /><root2 />)) should be(trim(<root1 /><root2 />))
    }
  }
}
