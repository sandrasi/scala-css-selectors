package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

import scala.xml.NodeSeq

class PrefixAttributeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/prefix_attribute_selector.xml"

  describe("construction") {

    it("constructs a prefix attribute selector with an attribute name and a value in any namespace") {
      PrefixAttributeSelector("attr", "value") should be(PrefixAttributeSelector(AnyNamespace, "attr", "value"))
    }

    it("constructs a prefix attribute selector with an attribute name and a value in no namespace") {
      PrefixAttributeSelector(null.asInstanceOf[String], "attr", "value") should be(PrefixAttributeSelector(NoNamespace, "attr", "value"))
    }

    it("constructs a prefix attribute selector with an attribute name and a value in a namespace") {
      PrefixAttributeSelector("http://www.example.com", "attr", "value") should be(PrefixAttributeSelector(UriNamespace("http://www.example.com"), "attr", "value"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new PrefixAttributeSelector(null, "attr", "value") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new PrefixAttributeSelector(NoNamespace, null, "value") }.getMessage should be("requirement failed: name is required")
    }

    it("requires an attribute value") {
      intercept[IllegalArgumentException] { new PrefixAttributeSelector(NoNamespace, "attr", null) }.getMessage should be("requirement failed: value is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = PrefixAttributeSelector("attr", "value")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified attribute and value without a namespace") {
        PrefixAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="value_prefix" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        PrefixAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="value_prefix" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        PrefixAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="value_prefix" /><elem ns:attr="value_prefix" />))
      }

      it("does not select anything if the specified attribute value is empty") {
        PrefixAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified attribute and value without a namespace") {
        PrefixAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="value_prefix" /><elem ATTR="VALUE_PREFIX" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        PrefixAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="value_prefix" /><elem ns:ATTR="VALUE_PREFIX" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        PrefixAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="value_prefix" /><elem ATTR="VALUE_PREFIX" /><elem ns:attr="value_prefix" /><elem ns:ATTR="VALUE_PREFIX" />))
      }

      it("does not select anything if the specified attribute value is empty") {
        PrefixAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }
  }
}
