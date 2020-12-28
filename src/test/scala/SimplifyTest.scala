import org.scalatest.FunSuite

class SimplifyTest extends FunSuite {

  // ------------------------------------------------------------
  // Addition
  // ------------------------------------------------------------

  test("x + 0 => x") {
    testSimp("x + 0", "x")
  }

  test("0 + x => x") {
    testSimp("0 + x", "x")
  }

  test("x + x => 2x") {
    testSimp("x + x", "(2.0*x)")
  }

  test("1 + 1 => 2") {
    testSimp("1 + 1", "2.0")
  }

  test("x + y => x + y") {
    testSimp("x + y", "(x+y)")
  }

  // ------------------------------------------------------------
  // Subtraction
  // ------------------------------------------------------------

  test("x - 0 => x") {
    testSimp("x - 0", "x")
  }

  test("0 - x => -x") {
    testSimp("0 - x", "neg(x)")
  }

  test("x - x => 0") {
    testSimp("x - x", "0.0")
  }

  test("1 - 2 => -1") {
    testSimp("1 - 2", "neg(1.0)")
  }

  test("x - y => x - y") {
    testSimp("x - y", "(x-y)")
  }

  // ------------------------------------------------------------
  // Multiplication
  // ------------------------------------------------------------


  test("x * 0 => 0") {
    testSimp("x * 0", "0.0")
  }

  test("0 * x => 0") {
    testSimp("0 * x", "0.0")
  }

  test("x * 1 => x") {
    testSimp("x * 1", "x")
  }

  test("1 * x => x") {
    testSimp("1 * x", "x")
  }

  test("3 * 2 => 6") {
    testSimp("3 * 2", "6.0")
  }

  test("x * x => x^2") {
    testSimp("x * x", "(x^2.0)")
  }

  test("x * y => x * y") {
    testSimp("x * y", "(x*y)")
  }

  // ------------------------------------------------------------
  // Division
  // ------------------------------------------------------------

  test("0 / x => 0") {
    testSimp("0 / x", "0.0")
  }

  test("x / x => 1") {
    testSimp("x / x", "1.0")
  }

  test("x / 1 => x") {
    testSimp("x / 1", "x")
  }

  test("x / 3 => x / 3") {
    testSimp("x / 3", "(x/3.0)")
  }

  test("6 / 3 => 2") {
    testSimp("6 / 3", "2.0")
  }

  // ------------------------------------------------------------
  // Exponentiation
  // ------------------------------------------------------------

  test("0^x => 0") {
    testSimp("0 ^ x", "0.0")
  }

  test("1^x => 1") {
    testSimp("1 ^ x", "1.0")
  }

  test("x^0 => 1") {
    testSimp("x ^ 0", "1.0")
  }

  test("x^1 => x") {
    testSimp("x ^ 1", "x")
  }

  test("e^ln(x) => x") {
    testSimp("e ^ ln(x)", "x")
  }

  test("10^log(x) => x") {
    testSimp("10 ^ log(x)", "x")
  }

  test("2^2 => 4") {
    testSimp("2 ^ 2", "4.0")
  }

  test("x^x => x^x") {
    testSimp("x ^ x", "(x^x)")
  }

  // ------------------------------------------------------------
  // Natural Log
  // ------------------------------------------------------------

  test("ln(1) => 0") {
    testSimp("ln(1)", "0.0")
  }

  test("ln(e) => 1") {
    testSimp("ln(e)", "1.0")
  }

  test("ln(e^x) => x") {
    testSimp("ln(e^x)", "x")
  }

  test("ln(3 * x) => ln(3 * x)") {
    testSimp("ln(3 * x)", "ln((3.0*x))")
  }

  // ------------------------------------------------------------
  // Log 10
  // ------------------------------------------------------------

  test("log(1) => 0") {
    testSimp("log(1)", "0.0")
  }

  test("log(10) => 1") {
    testSimp("log(10)", "1.0")
  }

  test("log(10^x) => x") {
    testSimp("log(10^x)", "x")
  }

  test("log(3 * x) => log(3 * x)") {
    testSimp("log(3 * x)", "log((3.0*x))")
  }

  // ------------------------------------------------------------
  // Square Root
  // ------------------------------------------------------------

  test("sqrt(x^2) => x") {
    testSimp("sqrt(x^2)", "x")
  }

  test("sqrt(x^3) => sqrt(x^3)") {
    testSimp("sqrt(x^3)", "sqrt((x^3.0))")
  }

  test("sqrt(x^4) => x^2") {
    testSimp("sqrt(x^4)", "(x^2.0)")
  }

  test("sqrt(4) => 2") {
    testSimp("sqrt(4)", "2.0")
  }

  test("sqrt(x * 35) => sqrt(x * 35)") {
    testSimp("sqrt(x * 35)", "sqrt((x*35.0))")
  }

  // ------------------------------------------------------------
  // Helpers
  // ------------------------------------------------------------
  def testSimp(expression: String, expected: String): Unit = {
    val result: ASTree = Parser.parse(expression).getOrElse(Empty).simplify
    assert(result.toString == expected)
  }
}
