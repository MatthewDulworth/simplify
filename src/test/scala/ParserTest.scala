import org.scalatest.{FunSuite, Tag}

class ParserTest extends FunSuite {

  test("empty input") {
    testParser("", None)
  }

  test("simple binary operations")(pending)

  test("subtract a negative number")(pending)

  test("simple parentheses")(pending)

  test("negative exponents")(pending)

  test("variables and functions")(pending)

  test("complex expression 1")(pending)

  test("complex expression 2")(pending)

  test("more negatives")(pending)

  test("still more negatives")(pending)

  test("toString")(pending)

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
