import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("test") {
    val res = Parser.parse("3 / 4 + 2")
    System.out.println(res)
  }

  test("empty input")(pending)

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
}
