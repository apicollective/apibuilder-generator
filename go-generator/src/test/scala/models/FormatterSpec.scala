package go.models

import Formatter.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class FormatterSpec extends AnyFunSpec with Matchers {

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
a b
c   d
""".trim.table() should be("""
// This is a comment
a b
c d
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
    "foo".indentString(0) should be("foo")
    "foo".indentString(1) should be("\tfoo")
    "foo".indentString(2) should be("\t\tfoo")
  }

}
