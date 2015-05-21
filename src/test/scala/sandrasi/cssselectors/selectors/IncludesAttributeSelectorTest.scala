package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

import scala.xml.NodeSeq

class IncludesAttributeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/includes_attribute_selector.xml"

  describe("construction") {

    it("constructs an includes attribute selector with an attribute name and a value in any namespace") {
      IncludesAttributeSelector("attr", "value") should be(IncludesAttributeSelector(AnyNamespace, "attr", "value"))
    }

    it("constructs an includes attribute selector with an attribute name and a value in no namespace") {
      IncludesAttributeSelector(null.asInstanceOf[String], "attr", "value") should be(IncludesAttributeSelector(NoNamespace, "attr", "value"))
    }

    it("constructs an exact attribute selector with an attribute name and a value in a namespace") {
      IncludesAttributeSelector("http://www.example.com", "attr", "value") should be(IncludesAttributeSelector(UriNamespace("http://www.example.com"), "attr", "value"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new IncludesAttributeSelector(null, "attr", "value") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new IncludesAttributeSelector(NoNamespace, null, "value") }.getMessage should be("requirement failed: name is required")
    }

    it("requires an attribute value") {
      intercept[IllegalArgumentException] { new IncludesAttributeSelector(NoNamespace, "attr", null) }.getMessage should be("requirement failed: value is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = IncludesAttributeSelector("attr", "value")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified attribute and value without a namespace") {
        IncludesAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="includes the value word" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        IncludesAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="includes the value word" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        IncludesAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="includes the value word" /><elem ns:attr="includes the value word" />))
      }

      it("does not select anything if the specified attribute value is empty") {
        IncludesAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified attribute and value without a namespace") {
        IncludesAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="includes the value word" /><elem ATTR="INCLUDES THE VALUE WORD" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        IncludesAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="includes the value word" /><elem ns:ATTR="INCLUDES THE VALUE WORD" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        IncludesAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="includes the value word" /><elem ATTR="INCLUDES THE VALUE WORD" /><elem ns:attr="includes the value word" /><elem ns:ATTR="INCLUDES THE VALUE WORD" />))
      }

      it("does not select anything if the specified attribute value is empty") {
        IncludesAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }
  }
}
