package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

import scala.xml.NodeSeq

class NthLastChildSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/nth_last_child_selector.xml"

  describe("simplification") {

    it("can't be further simplified") {
      val subject = NthLastChildSelector(0, 0)
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects nothing if a < 0 and b < 0") {
      NthLastChildSelector(-1, -1).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects nothing if a < 0 and b = 0") {
      NthLastChildSelector(-1, 0).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects the last child elements with positive index equal to an + b for all n ≥ 0 if a < 0 and b > 0") {
      NthLastChildSelector(-1, 1).select(testXml) should be(trim(<elem idx="4" />))
    }

    it("selects nothing if a = 0 and b < 0") {
      NthLastChildSelector(0, -1).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects nothing if a = 0 and b = 0") {
      NthLastChildSelector(0, 0).select(testXml) should be(NodeSeq.Empty)
    }

    it("selects the bth last child element if a = 0 and b > 0") {
      NthLastChildSelector(0, 1).select(testXml) should be(trim(<elem idx="4" />))
    }

    it("selects the last child elements with positive index equal to an + b for all n ≥ 0 if a > 0 and b < 0") {
      NthLastChildSelector(1, -1).select(testXml) should be(trim(<elem idx="1" /><elem idx="2" /><elem idx="3" /><elem idx="4" />))
    }

    it("selects the last child elements with index equal to an + b for all n ≥ 0 if a > 0 and b = 0") {
      NthLastChildSelector(1, 0).select(testXml) should be(trim(<elem idx="1" /><elem idx="2" /><elem idx="3" /><elem idx="4" />))
    }

    it("selects the last child elements with index equal to an + b for all n ≥ 0 if a > 0 and b > 0") {
      NthLastChildSelector(1, 1).select(testXml) should be(trim(<elem idx="1" /><elem idx="2" /><elem idx="3" /><elem idx="4" />))
    }
  }
}
