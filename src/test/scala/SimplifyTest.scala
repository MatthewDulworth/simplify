import org.scalatest.FunSuite

class SimplifyTest extends FunSuite {

  // ------------------------------------------------------------
  // Addition
  // ------------------------------------------------------------

  test("1 + 1") {
    testSimp("1 + 1", "2.0")
  }

  test("0 + e") {
    testSimp("e", "e")
  }

  test("pi + 0") {
    testSimp("pi", "pi")
  }

  test("x + x") {
    testSimp("x + x", "(x+x)")
  }

  // ------------------------------------------------------------
  // Subtraction
  // ------------------------------------------------------------

  test("1 - 2") {
    testSimp("1 - 2", "-1.0")
  }

  test("pi - 0") {
    testSimp("pi - 0", "pi")
  }

  test("0 - pi") {
    testSimp("0 - pi", "neg(pi)")
  }

  test("x - x") {
    testSimp("x - x", "0.0")
  }

  test("x - y") {
    testSimp("x - y", "(x-y)")
  }

  // ------------------------------------------------------------
  // Helpers
  // ------------------------------------------------------------
  def testSimp(expression: String, expected: String): Unit = {
    val result: ASTree = Parser.parse(expression).getOrElse(Empty).simplify
    assert(result.toString == expected)
  }
}