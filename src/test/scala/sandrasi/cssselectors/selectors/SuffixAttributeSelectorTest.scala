package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

import scala.xml.NodeSeq

class SuffixAttributeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/suffix_attribute_selector.xml"

  describe("construction") {

    it("constructs a suffix attribute selector with an attribute name and a value in any namespace") {
      SuffixAttributeSelector("attr", "value") should be(SuffixAttributeSelector(AnyNamespace, "attr", "value"))
    }

    it("constructs a suffix attribute selector with an attribute name and a value in no namespace") {
      SuffixAttributeSelector(null.asInstanceOf[String], "attr", "value") should be(SuffixAttributeSelector(NoNamespace, "attr", "value"))
    }

    it("constructs a suffix attribute selector with an attribute name and a value in a namespace") {
      SuffixAttributeSelector("http://www.example.com", "attr", "value") should be(SuffixAttributeSelector(UriNamespace("http://www.example.com"), "attr", "value"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new SuffixAttributeSelector(null, "attr", "value") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new SuffixAttributeSelector(NoNamespace, null, "value") }.getMessage should be("requirement failed: name is required")
    }

    it("requires an attribute value") {
      intercept[IllegalArgumentException] { new SuffixAttributeSelector(NoNamespace, "attr", null) }.getMessage should be("requirement failed: value is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = SuffixAttributeSelector("attr", "value")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified attribute and value without a namespace") {
        SuffixAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="suffix_value" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        SuffixAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="suffix_value" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        SuffixAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="suffix_value" /><elem ns:attr="suffix_value" />))
      }

      it("does not select anything if the specified attribute value is empty") {
        SuffixAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified attribute and value without a namespace") {
        SuffixAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="suffix_value" /><elem ATTR="SUFFIX_VALUE" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        SuffixAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="suffix_value" /><elem ns:ATTR="SUFFIX_VALUE" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        SuffixAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="suffix_value" /><elem ATTR="SUFFIX_VALUE" /><elem ns:attr="suffix_value" /><elem ns:ATTR="SUFFIX_VALUE" />))
      }

      it("does not select anything if the specified attribute value is empty") {
        SuffixAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }
  }
}
