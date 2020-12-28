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
  // Helpers
  // ------------------------------------------------------------
  def testSimp(expression: String, expected: String): Unit = {
    val result: ASTree = Parser.parse(expression).getOrElse(Empty).simplify
    assert(result.toString == expected)
  }
}
