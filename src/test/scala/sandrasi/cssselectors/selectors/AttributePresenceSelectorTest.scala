package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

class AttributePresenceSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/attribute_presence_selector.xml"

  describe("construction") {

    it("constructs an attribute presence selector with an attribute name in any namespace") {
      AttributePresenceSelector("attr") should be(AttributePresenceSelector(AnyNamespace, "attr"))
    }

    it("constructs an attribute presence selector with an attribute name in no namespace") {
      AttributePresenceSelector(null.asInstanceOf[String], "attr") should be(AttributePresenceSelector(NoNamespace, "attr"))
    }

    it("constructs an attribute presence selector with an attribute name in a namespace") {
      AttributePresenceSelector("http://www.example.com", "attr") should be(AttributePresenceSelector(UriNamespace("http://www.example.com"), "attr"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new AttributePresenceSelector(null, "attr") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new AttributePresenceSelector(NoNamespace, null) }.getMessage should be("requirement failed: name is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = AttributePresenceSelector("attr")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified attribute without a namespace") {
        AttributePresenceSelector(NoNamespace, "attr").select(testXml) should be(trim(<elem attr="" />))
      }

      it("selects all elements with the specified attribute from the specified namespace") {
        AttributePresenceSelector("http://www.example.com", "attr").select(testXml) should be(trim(<elem ns:attr="" />))
      }

      it("selects all elements with the specified attribute from all namespaces") {
        AttributePresenceSelector("attr").select(testXml) should be(trim(<elem attr="" /><elem ns:attr="" />))
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified attribute without a namespace") {
        AttributePresenceSelector(NoNamespace, "attr").select(trim(testXml)) should be(trim(<elem attr="" /><elem ATTR="VALUE" />))
      }

      it("selects all elements with the specified attribute from the specified namespace") {
        AttributePresenceSelector("http://www.example.com", "attr").select(testXml) should be(trim(<elem ns:attr="" /><elem ns:ATTR="VALUE" />))
      }

      it("selects all elements with the specified attribute from all namespaces") {
        AttributePresenceSelector("attr").select(testXml) should be(trim(<elem attr="" /><elem ATTR="VALUE" /><elem ns:attr="" /><elem ns:ATTR="VALUE" />))
      }
    }
  }
}
