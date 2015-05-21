package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

class ExactAttributeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/exact_attribute_selector.xml"

  describe("construction") {

    it("constructs an exact attribute selector with an attribute name and a value in any namespace") {
      ExactAttributeSelector("attr", "value") should be(ExactAttributeSelector(AnyNamespace, "attr", "value"))
    }

    it("constructs an exact attribute selector with an attribute name and a value in no namespace") {
      ExactAttributeSelector(null.asInstanceOf[String], "attr", "value") should be(ExactAttributeSelector(NoNamespace, "attr", "value"))
    }

    it("constructs an exact attribute selector with an attribute name and a value in a namespace") {
      ExactAttributeSelector("http://www.example.com", "attr", "value") should be(ExactAttributeSelector(UriNamespace("http://www.example.com"), "attr", "value"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new ExactAttributeSelector(null, "attr", "value") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new ExactAttributeSelector(NoNamespace, null, "value") }.getMessage should be("requirement failed: name is required")
    }

    it("requires an attribute value") {
      intercept[IllegalArgumentException] { new ExactAttributeSelector(NoNamespace, "attr", null) }.getMessage should be("requirement failed: value is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = ExactAttributeSelector("attr", "value")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified attribute and value without a namespace") {
        ExactAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="value" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        ExactAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="value" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        ExactAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="value" /><elem ns:attr="value" />))
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified attribute and value without a namespace") {
        ExactAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="value" /><elem ATTR="VALUE" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        ExactAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="value" /><elem ns:ATTR="VALUE" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        ExactAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="value" /><elem ATTR="VALUE" /><elem ns:attr="value" /><elem ns:ATTR="VALUE" />))
      }
    }
  }
}
