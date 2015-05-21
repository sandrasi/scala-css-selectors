package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

class DashAttributeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/dash_attribute_selector.xml"

  describe("construction") {

    it("constructs a dash attribute selector with an attribute name and a value in any namespace") {
      DashAttributeSelector("attr", "value") should be(DashAttributeSelector(AnyNamespace, "attr", "value"))
    }

    it("constructs a dash attribute selector with an attribute name and a value in no namespace") {
      DashAttributeSelector(null.asInstanceOf[String], "attr", "value") should be(DashAttributeSelector(NoNamespace, "attr", "value"))
    }

    it("constructs a dash attribute selector with an attribute name and a value in a namespace") {
      DashAttributeSelector("http://www.example.com", "attr", "value") should be(DashAttributeSelector(UriNamespace("http://www.example.com"), "attr", "value"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new DashAttributeSelector(null, "attr", "value") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new DashAttributeSelector(NoNamespace, null, "value") }.getMessage should be("requirement failed: name is required")
    }

    it("requires an attribute value") {
      intercept[IllegalArgumentException] { new DashAttributeSelector(NoNamespace, "attr", null) }.getMessage should be("requirement failed: value is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = DashAttributeSelector("attr", "value")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified attribute and value without a namespace") {
        DashAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="value" /><elem attr="value-something" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        DashAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="value" /><elem ns:attr="value-something" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        DashAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="value" /><elem attr="value-something" /><elem ns:attr="value" /><elem ns:attr="value-something" />))
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified attribute and value without a namespace") {
        DashAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="value" /><elem attr="value-something" /><elem ATTR="VALUE" /><elem ATTR="VALUE-SOMETHING" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        DashAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="value" /><elem ns:attr="value-something" /><elem ns:ATTR="VALUE" /><elem ns:ATTR="VALUE-SOMETHING" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        DashAttributeSelector("attr", "value").select(testXml) should be(
          trim(<elem attr="value" /><elem attr="value-something" /><elem ATTR="VALUE" /><elem ATTR="VALUE-SOMETHING" /><elem ns:attr="value" /><elem ns:attr="value-something" /><elem ns:ATTR="VALUE" /><elem ns:ATTR="VALUE-SOMETHING" />)
        )
      }
    }
  }
}
