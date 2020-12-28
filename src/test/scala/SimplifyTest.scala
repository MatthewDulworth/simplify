import org.scalatest.FunSuite

class SimplifyTest extends FunSuite {

  // ------------------------------------------------------------
  // Addition
  // ------------------------------------------------------------

  test("0 + e") {
    testSimp("e", "e")
  }

  test("pi + 0") {
    testSimp("pi", "pi")
  }

  test("1 + 1") {
    testSimp("1 + 1", "2.0")
  }

  test("x + x") {
    testSimp("x + x", "(2.0*x)")
  }

  test("x + y") {
    testSimp("x + y", "(x+y)")
  }

  // ------------------------------------------------------------
  // Subtraction
  // ------------------------------------------------------------

  test("1 - 2") {
    testSimp("1 - 2", "neg(1.0)")
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
  // Multiplication
  // ------------------------------------------------------------

  test("1 * 2") {
    testSimp("1 * 2", "2.0")
  }

  test("x * 0") {
    testSimp("x * 0", "0.0")
  }

  test("0 * x") {
    testSimp("0 * x", "0.0")
  }

  test("x * 1") {
    testSimp("x * 1", "x")
  }

  test("1 * x") {
    testSimp("1 * x", "x")
  }

  test("x * x") {
    testSimp("x * x", "(x^2.0)")
  }

  // ------------------------------------------------------------
  // Division
  // ------------------------------------------------------------

  test("0 / x") {
    testSimp("0 / x", "0.0")
  }

  test("x / x") {
    testSimp("x / x", "1.0")
  }

  test("x / 1") {
    testSimp("x / 1", "x")
  }

  test("x / 3") {
    testSimp("x / 3", "(x/3.0)")
  }

  test("6 / 3") {
    testSimp("6 / 3", "2.0")
  }

  // ------------------------------------------------------------
  // Exponentiation
  // ------------------------------------------------------------

  test("0 ^ x") {
    testSimp("0 ^ x", "0.0")
  }

  test("1 ^ x") {
    testSimp("1 ^ x", "1.0")
  }

  test("x ^ 0") {
    testSimp("x ^ 0", "1.0")
  }

  test("x ^ 1") {
    testSimp("x ^ 1", "x")
  }

  test("e ^ ln(x)") {
    testSimp("e ^ ln(x)", "x")
  }

  test("10 ^ log(x)") {
    testSimp("10 ^ log(x)", "x")
  }

  test("2 ^ 2") {
    testSimp("2 ^ 2", "4.0")
  }

  test("x ^ x") {
    testSimp("x ^ x", "(x^x)")
  }

  // ------------------------------------------------------------
  // Natural Log
  // ------------------------------------------------------------

  test("ln(1)") {
    testSimp("ln(1)", "0.0")
  }

  test("ln(e)") {
    testSimp("ln(e)", "1.0")
  }

  test("ln(e^x)") {
    testSimp("ln(e^x)", "x")
  }

  test("ln(3 * x)") {
    testSimp("ln(3 * x)", "ln((3.0*x))")
  }

  // ------------------------------------------------------------
  // Log 10
  // ------------------------------------------------------------

  test("log(1)") {
    testSimp("log(1)", "0.0")
  }

  test("log(10)") {
    testSimp("log(10)", "1.0")
  }

  test("log(10^x)") {
    testSimp("log(10^x)", "x")
  }

  test("log(3 * x)") {
    testSimp("log(3 * x)", "log((3.0*x))")
  }

  // ------------------------------------------------------------
  // Helpers
  // ------------------------------------------------------------
  def testSimp(expression: String, expected: String): Unit = {
    val result: ASTree = Parser.parse(expression).getOrElse(Empty).simplify
    assert(result.toString == expected)
  }
}
