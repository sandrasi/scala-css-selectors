package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

import scala.xml.NodeSeq

class InvalidSelectorTest extends FunSpec with Matchers {

  describe("simplification") {

    it("can't be further simplified") {
      InvalidSelector.simplify should be theSameInstanceAs InvalidSelector
    }
  }

  describe("element selection") {

    it("selects nothing") {
      InvalidSelector.select(<xml />) should be(NodeSeq.Empty)
    }
  }
}
