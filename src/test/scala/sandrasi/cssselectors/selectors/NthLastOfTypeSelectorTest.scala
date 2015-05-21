package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

import scala.xml.NodeSeq

class NthLastOfTypeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/nth_last_of_type_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = NthLastOfTypeSelector(0, 0)
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects nothing if a < 0 and b < 0") {
      NthLastOfTypeSelector(-1, -1).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects nothing if a < 0 and b = 0") {
      NthLastOfTypeSelector(-1, 0).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects the last child elements of all types with positive index equal to an + b for all n ≥ 0 if a < 0 and b > 0") {
      NthLastOfTypeSelector(-1, 1).select(testXml) should be(trim(<elem1 idx="2" /><elem2 idx="2" />))
    }

    it("selects nothing if a = 0 and b < 0") {
      NthLastOfTypeSelector(0, -1).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects nothing if a = 0 and b = 0") {
      NthLastOfTypeSelector(0, 0).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects the bth last child element of all types if a = 0 and b > 0") {
      NthLastOfTypeSelector(0, 1).select(testXml) should be(trim(<elem1 idx="2" /><elem2 idx="2" />))
    }

    it("selects the last child elements of all types with positive index equal to an + b for all n ≥ 0 if a > 0 and b < 0") {
      NthLastOfTypeSelector(1, -1).select(testXml) should be(trim(<elem1 idx="1" /><elem2 idx="1" /><elem1 idx="2" /><elem2 idx="2" />))
    }

    it("selects the last child elements of all types with index equal to an + b for all n ≥ 0 if a > 0 and b = 0") {
      NthLastOfTypeSelector(1, 0).select(testXml) should be(trim(<elem1 idx="1" /><elem2 idx="1" /><elem1 idx="2" /><elem2 idx="2" />))
    }

    it("selects the last child elements of all types with index equal to an + b for all n ≥ 0 if a > 0 and b > 0") {
      NthLastOfTypeSelector(1, 1).select(testXml) should be(trim(<elem1 idx="1" /><elem2 idx="1" /><elem1 idx="2" /><elem2 idx="2" />))
    }
  }
}
