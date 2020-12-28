import org.scalatest.{FunSuite, Tag}

class ParserTest extends FunSuite {

  // ------------------------------------------------------------
  // Binary Operators
  // ------------------------------------------------------------

  test("empty input") {
    testParser("", None)
  }

  test("single number") {
    testParser("e", Some("e"))
  }

  // ------------------------------------------------------------
  // Simple Operators
  // ------------------------------------------------------------

  test("simple add") {
    testParser("3 + 4", Some("(3.0+4.0)"))
  }

  test("simple multiply") {
    testParser("e * pi", Some("(e*pi)"))
  }

  test("simple subtract") {
    testParser("e - 4", Some("(e-4.0)"))
  }

  test("simple divide") {
    testParser("pi / 3", Some("(pi/3.0)"))
  }

  test("simple power") {
    testParser("2 ^ 3", Some("(2.0^3.0)"))
  }

  test("simple negate") {
    testParser("-pi", Some("neg(pi)"))
  }

  // ------------------------------------------------------------
  // Simple Expressions
  // ------------------------------------------------------------

  test("x + y / z") {
    testParser("x + y / z", Some("(x+(y/z))"))
  }

  test("x / y + z") {
    testParser("x / y + z", Some("((x/y)+z)"))
  }

  test("x - y + z") {
    testParser("x - y + z", Some("((x-y)+z)"))
  }

  test("x + y - z") {
    testParser("x + y - z", Some("((x+y)-z)"))
  }


  def testParser(expression: String, expected: Option[String]): Unit = {
    val actual = Parser.parse(expression)

    (actual, expected) match {
      case (None, None) =>
      case (None, _) => assert(actual.toString == expected.get.toString)
      case (_, None) => assert(actual.get.toString == expected.toString)
      case _ => assert(actual.get.toString == expected.get)
    }
  }
}
