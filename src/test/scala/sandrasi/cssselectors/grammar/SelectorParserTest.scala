package sandrasi.cssselectors.grammar

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.selectors._
import sandrasi.cssselectors.util.NoNamespace

class SelectorParserTest extends FunSpec with Matchers {

  private def subject(implicit defaultNamespace: String = null, namespaces: Map[String, String] = Map.empty) = {
    new SelectorParser(Option(defaultNamespace), namespaces)
  }

  describe("validation") {

    it("requires a default namespace") {
      intercept[IllegalArgumentException] { new SelectorParser(null, Map.empty) }.getMessage should be("requirement failed: defaultNamespace is required")
    }

    it("requires namespaces") {
      intercept[IllegalArgumentException] { new SelectorParser(None, null) }.getMessage should be("requirement failed: namespaces is required")
    }
  }

  describe("simple selectors") {

    describe("element selectors") {

      describe("universal selector") {

        describe("without namespace prefix") {

          it("parses ‘*’ to universal selector in any namespace") {
            subject.parse("*") should be(UniversalSelector())
          }

          describe("with default namespace") {

            implicit val defaultNamespace = "http://www.example.com"

            it("parses ‘*’ to universal selector in the default namespace") {
              subject.parse("*") should be(UniversalSelector("http://www.example.com"))
            }
          }
        }

        describe("with namespace prefix") {

          it("parses ‘|*’ to universal selector in no namespace") {
            subject.parse("|*") should be(UniversalSelector(NoNamespace))
          }

          describe("with namespace") {

            implicit val namespaces = Map("ns" -> "http://www.example.com")

            it("parses ‘ns|*’ to universal selector in the ‘ns’ namespace") {
              subject.parse("ns|*") should be(UniversalSelector("http://www.example.com"))
            }

            it( """parses ‘\n\s|*’ to universal selector in the ‘ns’ namespace""") {
              subject.parse("""\n\s|*""") should be(UniversalSelector("http://www.example.com"))
            }

            it( """parses ‘\6e\73|*’ to universal selector in the ‘ns’ namespace""") {
              subject.parse("""\6e\73|*""") should be(UniversalSelector("http://www.example.com"))
            }
          }

          it("parses ‘ns|*’ to invalid selector if ‘ns’ is not a specified namespace") {
            subject.parse("ns|*") should be(InvalidSelector)
          }
        }
      }

      describe("type selector") {

        describe("without namespace prefix") {

          it("parses ‘T’ to type selector in any namespace") {
            subject.parse("T") should be(TypeSelector("T"))
          }

          it( """parses ‘\T’ to type selector in any namespace""") {
            subject.parse("""\T""") should be(TypeSelector("T"))
          }

          it( """parses ‘\54’ to type selector in any namespace""") {
            subject.parse("""\54""") should be(TypeSelector("T"))
          }

          it("parses ‘|T’ to type selector in no namespace") {
            subject.parse("|T") should be(TypeSelector(NoNamespace, "T"))
          }

          describe("with default namespace") {

            implicit val defaultNamespace = "http://www.example.com"

            it("parses ‘T’ to type selector in the default namespace") {
              subject.parse("T") should be(TypeSelector("http://www.example.com", "T"))
            }
          }
        }

        describe("with namespace prefix") {

          describe("with namespace") {

            implicit val namespaces = Map("ns" -> "http://www.example.com")

            it("parses ‘ns|T’ to type selector in the ‘ns’ namespace") {
              subject.parse("ns|T") should be(TypeSelector("http://www.example.com", "T"))
            }

            it( """parses ‘\n\s|\T’ to type selector in the ‘ns’ namespace""") {
              subject.parse("""\n\s|\T""") should be(TypeSelector("http://www.example.com", "T"))
            }

            it( """parses ‘\6e\73|\54’ to type selector in the ‘ns’ namespace""") {
              subject.parse("""\6e\73|\54""") should be(TypeSelector("http://www.example.com", "T"))
            }
          }

          it("parses ‘ns|T’ to invalid selector if ‘ns’ is not a specified namespace") {
            subject.parse("ns|T") should be(InvalidSelector)
          }
        }
      }
    }

    describe("class selector") {

      it("parses ‘.class’ to class selector") {
        subject.parse(".class") should be(ClassSelector("class"))
      }

      it( """parses ‘.c\la\s\s’ to class selector""") {
        subject.parse(""".c\la\s\s""") should be(ClassSelector("class"))
      }

      it( """parses ‘.\63\6c\61\73\73’ to class selector""") {
        subject.parse(""".\63\6c\61\73\73""") should be(ClassSelector("class"))
      }
    }

    describe("id selector") {

      it("parses ‘#id’ to id selector") {
        subject.parse("#id") should be(IdSelector("id"))
      }

      it( """parses ‘#\id’ to id selector""") {
        subject.parse("""#\id""") should be(IdSelector("id"))
      }

      it( """parses ‘#\69\64’ to id selector""") {
        subject.parse("""#\69\64""") should be(IdSelector("id"))
      }
    }

    describe("attribute selectors") {

      describe("attribute presence selector") {

        describe("without namespace prefix") {

          it("parses ‘[attr]’ to attribute presence selector in any namespace") {
            subject.parse("[attr]") should be(AttributePresenceSelector("attr"))
          }

          it( """parses ‘[a\t\t\r]’ to attribute presence selector in any namespace""") {
            subject.parse("""[a\t\t\r]""") should be(AttributePresenceSelector("attr"))
          }

          it( """parses ‘[\61\74\74\72]’ to attribute presence selector in any namespace""") {
            subject.parse("""[\61\74\74\72]""") should be(AttributePresenceSelector("attr"))
          }

          it("parses ‘[|attr]’ to attribute presence selector in no namespace") {
            subject.parse("[|attr]") should be(AttributePresenceSelector(NoNamespace, "attr"))
          }

          describe("with default namespace") {

            implicit val defaultNamespace = "http://www.example.com"

            it("parses ‘[attr]’ to attribute presence selector in any namespace") {
              subject.parse("[attr]") should be(AttributePresenceSelector("attr"))
            }
          }
        }

        describe("with namespace prefix") {

          describe("with namespace") {

            implicit val namespaces = Map("ns" -> "http://www.example.com")

            it("parses ‘[ns|attr]’ to attribute presence selector in the ‘ns’ namespace") {
              subject.parse("[ns|attr]") should be(AttributePresenceSelector("http://www.example.com", "attr"))
            }

            it("""parses ‘[\n\s|a\t\t\r]’ to attribute presence selector in the ‘ns’ namespace""") {
              subject.parse("""[\n\s|a\t\t\r]""") should be(AttributePresenceSelector("http://www.example.com", "attr"))
            }

            it("""parses ‘[\6e\73|\61\74\74\72]’ to attribute presence selector in the ‘ns’ namespace""") {
              subject.parse("""[\6e\73|\61\74\74\72]""") should be(AttributePresenceSelector("http://www.example.com", "attr"))
            }
          }

          it("parses ‘[ns|attr]’ to invalid selector if ‘ns’ is not a specified namespace") {
            subject.parse("[ns|T]") should be(InvalidSelector)
          }
        }
      }

      describe("prefix attribute selector") {

        describe("without namespace prefix") {

          it("parses ‘[attr^=value]’ to prefix attribute selector in any namespace") {
            subject.parse("[attr^=value]") should be(PrefixAttributeSelector("attr", "value"))
          }

          it( """parses ‘[a\t\t\r^=\va\lue]’ to prefix attribute selector in any namespace""") {
            subject.parse("""[a\t\t\r^=\va\lue]""") should be(PrefixAttributeSelector("attr", "value"))
          }

          it( """parses ‘[\61\74\74\72^=\76\61\6c\75\65]’ to prefix attribute selector in any namespace""") {
            subject.parse("""[\61\74\74\72^=\76\61\6c\75\65]""") should be(PrefixAttributeSelector("attr", "value"))
          }

          it("parses ‘[|attr^=value]’ to prefix attribute selector in no namespace") {
            subject.parse("[|attr^=value]") should be(PrefixAttributeSelector(NoNamespace, "attr", "value"))
          }

          describe("with default namespace") {

            implicit val defaultNamespace = "http://www.example.com"

            it("parses ‘[attr^=value]’ to prefix attribute selector in any namespace") {
              subject.parse("[attr^=value]") should be(PrefixAttributeSelector("attr", "value"))
            }
          }
        }

        describe("with namespace prefix") {

          describe("with namespace") {

            implicit val namespaces = Map("ns" -> "http://www.example.com")

            it("parses ‘[ns|attr^=value]’ to prefix attribute selector in the ‘ns’ namespace") {
              subject.parse("[ns|attr^=value]") should be(PrefixAttributeSelector("http://www.example.com", "attr", "value"))
            }

            it("""parses ‘[\n\s|a\t\t\r^=\va\lue]’ to prefix attribute selector in the ‘ns’ namespace""") {
              subject.parse("""[\n\s|a\t\t\r^=\va\lue]""") should be(PrefixAttributeSelector("http://www.example.com", "attr", "value"))
            }

            it("""parses ‘[\6e\73|\61\74\74\72^=\76\61\6c\75\65]’ to prefix attribute selector in the ‘ns’ namespace""") {
              subject.parse("""[\6e\73|\61\74\74\72^=\76\61\6c\75\65]""") should be(PrefixAttributeSelector("http://www.example.com", "attr", "value"))
            }
          }

          it("parses ‘[ns|attr]’ to invalid selector if ‘ns’ is not a specified namespace") {
            subject.parse("[ns|T]") should be(InvalidSelector)
          }
        }
      }
    }
  }

//  test("should parse ‘[attr^=value]’ as attribute selector with prefix match on an ‘ident’ value") {
//    subject.parse("[attr^=value]") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value"))))), None))))
//  }
//
//  test("should parse ‘[attr$=value]’ as attribute selector with suffix match on an ‘ident’ value") {
//    subject.parse("[attr$=value]") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value"))))), None))))
//  }
//
//  test("should parse ‘[attr*=value]’ as attribute selector with substring match on an ‘ident’ value") {
//    subject.parse("[attr*=value]") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value"))))), None))))
//  }
//
//  test("should parse ‘[attr=value]’ as attribute selector with exact match on an ‘ident’ value") {
//    subject.parse("[attr=value]") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value"))))), None))))
//  }
//
//  test("should parse ‘[attr~=value]’ as attribute selector with includes match on an ‘ident’ value") {
//    subject.parse("[attr~=value]") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value"))))), None))))
//  }
//
//  test("should parse ‘[attr|=value]’ as attribute selector with hyphen match on an ‘ident’ value") {
//    subject.parse("[attr|=value]") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value"))))), None))))
//  }
//
//  test("""should parse ‘[attr="value1 value2"]’ as attribute selector with a string value""") {
//    subject.parse("""[attr="value1 value2"]""") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value1 value2"))))), None))))
//  }
//
//  test("should parse ‘[attr='value1 value2']’ as attribute selector with a string value") {
//    subject.parse("[attr='value1 value2']") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value1 value2"))))), None))))
//  }
//
//  test("""should parse ‘[attr="\va\lue1\ \va\lue2"]’ as attribute selector with a string value""") {
//    subject.parse("""[attr="\va\lue1\ \va\lue2"]""") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value1 value2"))))), None))))
//  }
//
//  test("""should parse ‘[attr="\76\61\6c\75\65\31\20\76\61\6c\75\65\32"]’ as attribute selector with a string value""") {
//    subject.parse("""[attr="\76\61\6c\75\65\31\20\76\61\6c\75\65\32"]""") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("value1 value2"))))), None))))
//  }
//
//  test("""should parse ‘[attr="\"quoted value\""]’ as attribute selector with a string value containing double quotes""") {
//    subject.parse("""[attr="\"quoted value\""]""") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher(""""quoted value""""))))), None))))
//  }
//
//  test("""should parse ‘[attr='\'quoted value\'']’ as attribute selector with a string value containing single quotes""") {
//    subject.parse("""[attr='\'quoted value\'']""") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AttributeSelector(None, "attr", Some(new AttributeValueMatcher("'quoted value'"))))), None))))
//  }
//
//  test("should parse ‘:root’ as root pseudo-class selector") {
//    subject.parse(":root") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(RootSelector())), None))))
//  }
//
//  test("should parse ‘:first-child’ as first child pseudo-class selector") {
//    subject.parse(":first-child") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(FirstChildSelector())), None))))
//  }
//
//  test("should parse ‘:last-child’ as last child pseudo-class selector") {
//    subject.parse(":last-child") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(LastChildSelector())), None))))
//  }
//
//  test("should parse ‘:first-of-type’ as first of type pseudo-class selector") {
//    subject.parse(":first-of-type") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(FirstOfTypeSelector())), None))))
//  }
//
//  test("should parse ‘:last-of-type’ as last of type pseudo-class selector") {
//    subject.parse(":last-of-type") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(LastOfTypeSelector())), None))))
//  }
//
//  test("should parse ‘:only-child’ as only child pseudo-class selector") {
//    subject.parse(":only-child") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(OnlyChildSelector())), None))))
//  }
//
//  test("should parse ‘:only-of-type’ as only of type pseudo-class selector") {
//    subject.parse(":only-of-type") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(OnlyOfTypeSelector())), None))))
//  }
//
//  test("should parse ‘:empty’ as empty pseudo-class selector") {
//    subject.parse(":empty") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(EmptySelector())), None))))
//  }
//
//  test("should parse ‘:link’ as link pseudo-class selector") {
//    subject.parse(":link") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(LinkSelector())), None))))
//  }
//
//  test("should parse ‘:visited’ as visited pseudo-class selector") {
//    subject.parse(":visited") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(VisitedSelector())), None))))
//  }
//
//  test("should parse ‘:active’ as active pseudo-class selector") {
//    subject.parse(":active") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(ActiveSelector())), None))))
//  }
//
//  test("should parse ‘:hover’ as hover pseudo-class selector") {
//    subject.parse(":hover") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(HoverSelector())), None))))
//  }
//
//  test("should parse ‘:focus’ as focus pseudo-class selector") {
//    subject.parse(":focus") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(FocusSelector())), None))))
//  }
//
//  test("should parse ‘:target’ as target pseudo-class selector") {
//    subject.parse(":target") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(TargetSelector())), None))))
//  }
//
//  test("should parse ‘:enabled’ as enabled pseudo-class selector") {
//    subject.parse(":enabled") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(EnabledSelector())), None))))
//  }
//
//  test("should parse ‘:disabled’ as disabled pseudo-class selector") {
//    subject.parse(":disabled") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(DisabledSelector())), None))))
//  }
//
//  test("should parse ‘:checked’ as checked pseudo-class selector") {
//    subject.parse(":checked") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(CheckedSelector())), None))))
//  }
//
//  test("should parse ‘:nth-child(1)’ as n-th child functional pseudo-class selector without ‘a’ and with positive ‘b’ arguments") {
//    subject.parse(":nth-child(1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthChildSelector(NumberExpression(0, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-child(n)’ as n-th child functional pseudo-class selector with positive ‘a’ and without ‘b’ arguments") {
//    subject.parse(":nth-child(n)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthChildSelector(NumberExpression(1, 0)))), None))))
//  }
//
//  test("should parse ‘:nth-child(2n+1)’ as n-th child functional pseudo-class selector with positive ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-child(2n+1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthChildSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-child(-2n-1)’ as n-th child functional pseudo-class selector with negative ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-child(-2n-1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthChildSelector(NumberExpression(-2, -1)))), None))))
//  }
//
//  test("should parse ‘:nth-child(odd)’ as n-th child functional pseudo-class selector with odd argument") {
//    subject.parse(":nth-child(odd)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthChildSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-child(even)’ as n-th child functional pseudo-class selector with even argument") {
//    subject.parse(":nth-child(even)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthChildSelector(NumberExpression(2, 0)))), None))))
//  }
//
//  test("should parse ‘:nth-last-child(1)’ as n-th last child functional pseudo-class selector without ‘a’ and with positive ‘b’ arguments") {
//    subject.parse(":nth-last-child(1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastChildSelector(NumberExpression(0, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-child(n)’ as n-th last child functional pseudo-class selector with positive ‘a’ and without ‘b’ arguments") {
//    subject.parse(":nth-last-child(n)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastChildSelector(NumberExpression(1, 0)))), None))))
//  }
//
//  test("should parse ‘:nth-last-child(2n+1)’ as n-th last child functional pseudo-class selector with positive ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-last-child(2n+1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastChildSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-child(-2n-1)’ as n-th last child functional pseudo-class selector with negative ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-last-child(-2n-1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastChildSelector(NumberExpression(-2, -1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-child(odd)’ as n-th last child functional pseudo-class selector with odd argument") {
//    subject.parse(":nth-last-child(odd)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastChildSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-child(even)’ as n-th last child functional pseudo-class selector with even argument") {
//    subject.parse(":nth-last-child(even)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastChildSelector(NumberExpression(2, 0)))), None))))
//  }
//
//  test("should parse ‘:nth-of-type(1)’ as n-th child of type functional pseudo-class selector without ‘a’ and with positive ‘b’ arguments") {
//    subject.parse(":nth-of-type(1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthOfTypeSelector(NumberExpression(0, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-of-type(n)’ as n-th child of type functional pseudo-class selector with positive ‘a’ and without ‘b’ arguments") {
//    subject.parse(":nth-of-type(n)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthOfTypeSelector(NumberExpression(1, 0)))), None))))
//  }
//
//  test("should parse ‘:nth-of-type(2n+1)’ as n-th child of type functional pseudo-class selector with positive ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-of-type(2n+1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthOfTypeSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-of-type(-2n-1)’ as n-th child of type functional pseudo-class selector with negative ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-of-type(-2n-1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthOfTypeSelector(NumberExpression(-2, -1)))), None))))
//  }
//
//  test("should parse ‘:nth-of-type(odd)’ as n-th child of type functional pseudo-class selector with odd argument") {
//    subject.parse(":nth-of-type(odd)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthOfTypeSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-of-type(even)’ as n-th child of type functional pseudo-class selector with even argument") {
//    subject.parse(":nth-of-type(even)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthOfTypeSelector(NumberExpression(2, 0)))), None))))
//  }
//
//  test("should parse ‘:nth-last-of-type(1)’ as n-th last child of type functional pseudo-class selector without ‘a’ and with positive ‘b’ arguments") {
//    subject.parse(":nth-last-of-type(1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastOfTypeSelector(NumberExpression(0, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-of-type(n)’ as n-th last child of type functional pseudo-class selector with positive ‘a’ and without ‘b’ arguments") {
//    subject.parse(":nth-last-of-type(n)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastOfTypeSelector(NumberExpression(1, 0)))), None))))
//  }
//
//  test("should parse ‘:nth-last-of-type(2n+1)’ as n-th child of type functional pseudo-class selector with positive ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-last-of-type(2n+1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastOfTypeSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-of-type(-2n-1)’ as n-th child of type functional pseudo-class selector with negative ‘a’ and ‘b’ arguments") {
//    subject.parse(":nth-last-of-type(-2n-1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastOfTypeSelector(NumberExpression(-2, -1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-of-type(odd)’ as n-th child of type functional pseudo-class selector with odd argument") {
//    subject.parse(":nth-last-of-type(odd)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastOfTypeSelector(NumberExpression(2, 1)))), None))))
//  }
//
//  test("should parse ‘:nth-last-of-type(even)’ as n-th child of type functional pseudo-class selector with even argument") {
//    subject.parse(":nth-last-of-type(even)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NthLastOfTypeSelector(NumberExpression(2, 0)))), None))))
//  }
//
//  test("should parse ‘:lang(en-us)’ as language functional pseudo-class selector with an ‘ident’ argument") {
//    subject.parse(":lang(en-us)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(LangSelector("en-us"))), None))))
//  }
//
//  test("should parse ‘::first-line’ as first line pseudo-element selector") {
//    subject.parse("::first-line") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(FirstLineSelector())), None))))
//  }
//
//  test("should parse ‘::first-letter’ as first letter pseudo-element selector") {
//    subject.parse("::first-letter") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(FirstLetterSelector())), None))))
//  }
//
//  test("should parse ‘::before’ as before pseudo-element selector") {
//    subject.parse("::before") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(BeforeSelector())), None))))
//  }
//
//  test("should parse ‘::after’ as after pseudo-element selector") {
//    subject.parse("::after") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(AfterSelector())), None))))
//  }
//
//  test("should parse ‘:not(*)’ as negated universal selector") {
//    subject.parse(":not(*)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NotSelector(UniversalSelector(None)))), None))))
//  }
//
//  test("should parse ‘:not(T)’ as negated type selector") {
//    subject.parse(":not(T)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NotSelector(TypeSelector(None, "T")))), None))))
//  }
//
//  test("should parse ‘:not(.class)’ as negated class selector") {
//    subject.parse(":not(.class)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NotSelector(ClassSelector("class")))), None))))
//  }
//
//  test("should parse ‘:not(#id)’ as negated id selector") {
//    subject.parse(":not(#id)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NotSelector(IdSelector("id")))), None))))
//  }
//
//  test("should parse ‘:not([attr])’ as negated attribute selector") {
//    subject.parse(":not([attr])") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NotSelector(AttributeSelector(None, "attr", None)))), None))))
//  }
//
//  test("should parse ‘:not(:root)’ as negated pseudo-class selector") {
//    subject.parse(":not(:root)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NotSelector(RootSelector()))), None))))
//  }
//
//  test("should parse ‘:not(:nth-child(1))’ as negated functional pseudo-class selector") {
//    subject.parse(":not(:nth-child(1))") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(None, List(NotSelector(NthChildSelector(NumberExpression(0, 1))))), None))))
//  }
//
//  test("should parse ‘T.class’ as stacked type and class selector") {
//    subject.parse("T.class") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List(ClassSelector("class"))), None))))
//  }
//
//  test("should parse ‘T#id’ as stacked type and id selector") {
//    subject.parse("T#id") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List(IdSelector("id"))), None))))
//  }
//
//  test("should parse ‘T[attr]’ as stacked type and attribute selector") {
//    subject.parse("T[attr]") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List(AttributeSelector(None, "attr", None))), None))))
//  }
//
//  test("should parse ‘T:root’ as stacked type and pseudo-class selector") {
//    subject.parse("T:root") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List(RootSelector())), None))))
//  }
//
//  test("should parse ‘T:nth-child(1)’ as stacked type and functional pseudo-class selector") {
//    subject.parse("T:nth-child(1)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List(NthChildSelector(NumberExpression(0, 1)))), None))))
//  }
//
//  test("should parse ‘T::after’ as stacked type and pseudo-element selector") {
//    subject.parse("T::after") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List(AfterSelector())), None))))
//  }
//
//  test("should parse ‘T:not(S)’ as stacked type and negation selector") {
//    subject.parse("T:not(S)") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List(NotSelector(TypeSelector(None, "S")))), None))))
//  }
//
//  test("should parse ‘T S’ as the ‘S’ simple selector being the descendant of the ‘T’ simple selector") {
//    subject.parse("T S") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List()), Some(CombinedSelector(DescendantCombinator(), SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "S")), List()), None)))))))
//  }
//
//  test("should parse ‘T > S’ as the ‘S’ simple selector being the child of the ‘T’ simple selector") {
//    subject.parse("T > S") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List()), Some(CombinedSelector(ChildCombinator(), SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "S")), List()), None)))))))
//  }
//
//  test("should parse ‘T + S’ as the ‘S’ simple selector being the adjacent sibling of the ‘T’ simple selector") {
//    subject.parse("T + S") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List()), Some(CombinedSelector(AdjacentSiblingCombinator(), SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "S")), List()), None)))))))
//  }
//
//  test("should parse ‘T ~ S’ as the ‘S’ simple selector being the sibling of the ‘T’ simple selector") {
//    subject.parse("T ~ S") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List()), Some(CombinedSelector(GeneralSiblingCombinator(), SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "S")), List()), None)))))))
//  }
//
//  test("should parse ‘T, S’ as selector group") {
//    subject.parse("T, S") should be(GroupSelector(List(SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "T")), List()), None), SelectorCombination(SimpleSelectorSequence(Some(TypeSelector(None, "S")), List()), None))))
//  }
}
