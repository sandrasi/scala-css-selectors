package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

import scala.xml.NodeSeq

class SubstringAttributeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/substring_attribute_selector.xml"

  describe("construction") {

    it("constructs a substring attribute selector with an attribute name and a value in any namespace") {
      SubstringAttributeSelector("attr", "value") should be(SubstringAttributeSelector(AnyNamespace, "attr", "value"))
    }

    it("constructs a substring attribute selector with an attribute name and a value in no namespace") {
      SubstringAttributeSelector(null.asInstanceOf[String], "attr", "value") should be(SubstringAttributeSelector(NoNamespace, "attr", "value"))
    }

    it("constructs a substring attribute selector with an attribute name and a value in a namespace") {
      SubstringAttributeSelector("http://www.example.com", "attr", "value") should be(SubstringAttributeSelector(UriNamespace("http://www.example.com"), "attr", "value"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new SubstringAttributeSelector(null, "attr", "value") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new SubstringAttributeSelector(NoNamespace, null, "value") }.getMessage should be("requirement failed: name is required")
    }

    it("requires an attribute value") {
      intercept[IllegalArgumentException] { new SubstringAttributeSelector(NoNamespace, "attr", null) }.getMessage should be("requirement failed: value is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = SubstringAttributeSelector("attr", "value")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified attribute and value without a namespace") {
        SubstringAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="contains_value_substring" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        SubstringAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="contains_value_substring" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        SubstringAttributeSelector("attr", "value").select(testXml) should be(trim(<elem attr="contains_value_substring" /><elem ns:attr="contains_value_substring" />))
      }

      it("does not select anything if the specified attribute value is empty") {
        SubstringAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified attribute and value without a namespace") {
        SubstringAttributeSelector(NoNamespace, "attr", "value").select(testXml) should be(trim(<elem attr="contains_value_substring" /><elem ATTR="CONTAINS_VALUE_SUBSTRING" />))
      }

      it("selects all elements with the specified attribute and value from the specified namespace") {
        SubstringAttributeSelector("http://www.example.com", "attr", "value").select(testXml) should be(trim(<elem ns:attr="contains_value_substring" /><elem ns:ATTR="CONTAINS_VALUE_SUBSTRING" />))
      }

      it("selects all elements with the specified attribute and value from all namespaces") {
        SubstringAttributeSelector("attr", "value").select(testXml) should be(
          trim(<elem attr="contains_value_substring" /><elem ATTR="CONTAINS_VALUE_SUBSTRING" /><elem ns:attr="contains_value_substring" /><elem ns:ATTR="CONTAINS_VALUE_SUBSTRING" />)
        )
      }

      it("does not select anything if the specified attribute value is empty") {
        SubstringAttributeSelector("attr", "").select(testXml) should be(NodeSeq.Empty)
      }
    }
  }
}
