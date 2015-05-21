package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration

class IdSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/id_selector.xml"

  describe("validation") {

    it("requires an identifier") {
      intercept[IllegalArgumentException] { IdSelector(null) }.getMessage should be("requirement failed: identifier is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = IdSelector("id")
      subject.simplify shouldBe theSameInstanceAs(subject)
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects the element with the specified identifier where the id attribute is not in any namespace") {
        IdSelector("id1").select(testXml) should be(trim(<elem id="id1" />))
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects the element with the specified identifier where the id attribute is not in any namespace") {
        IdSelector("id2").select(testXml) should be(trim(<elem ID="ID2" />))
      }
    }
  }
}
