package go.models

import org.scalatest.{FunSpec, Matchers}
import Formatter._

class FormatterSpec extends FunSpec with Matchers {

  it("assignment") {
    """
CalendarWeekdays Calendar = "Weekdays"
CalendarEveryday = "Everyday"
""".trim.table() should be("""
CalendarWeekdays Calendar  = "Weekdays"
CalendarEveryday           = "Everyday"
""".trim)
  }

  it("leaves comments alone") {
    """
// This is a comment
CalendarWeekdays Calendar = "Weekdays"
CalendarEveryday = "Everyday"
""".trim.table() should be("""
// This is a comment
CalendarWeekdays Calendar  = "Weekdays"
CalendarEveryday           = "Everyday"
""".trim)
  }

  it("table") {
    "a".table() should be("a")
    "a b".table() should be("a b")

    """
a b
c   d
""".trim.table() should be("""
a b
c d
""".trim)

    """
a           b
foo
c  d
""".trim.table() should be("""
a   b
foo
c   d
""".trim)
  }

  it("table w/ 3 columns") {
    """
a b
a   b   c
apple bat cat
""".trim.table() should be("""
a     b
a     b   c
apple bat cat
""".trim)
  }

  it("table preserves leading spaces") {
    "  a".table() should be("  a")
  }

  it("indent") {
    "foo"indent(0) should be("foo")
    "foo"indent(1) should be("\tfoo")
    "foo"indent(2) should be("\t\tfoo")
  }

}
