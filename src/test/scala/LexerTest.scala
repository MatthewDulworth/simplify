import org.scalatest.FunSuite

class LexerTest extends FunSuite {

  test("empty input") {
    testLexer("", Nil)
  }

  test("whitespace") {
    testLexer("         \r  \r ", Nil)
  }

  // ------------------------------------------------------------
  // Numbers
  // ------------------------------------------------------------

  test("decimal numbers") {
    testLexer("3.04420", Decimal(3.04420) :: Nil)
    testLexer(".0", Decimal(0) :: Nil)
  }

  test("variables") {
    testLexer("xasd", Variable("xasd") :: Nil)
  }

  test("named constants") {
    testLexer("pi", PI :: Nil)
    testLexer("e", E :: Nil)
  }

  test("invalid numbers") {
    testLexer("3.2424.5", InvalidNumber("3.2424.5") :: Nil)
    testLexer(".", InvalidNumber(".") :: Nil)
  }

  test("all numbers") {
    testLexer("3.13pi0.0e2.1111x9uy",
      Decimal(3.13) :: PI :: Decimal(0) :: E :: Decimal(2.1111) :: Variable("x") :: Decimal(9) :: Variable("uy") :: Nil
    )
  }

  // ------------------------------------------------------------
  // Operators
  // ------------------------------------------------------------

  test("binary operators") {
    testLexer("e + e", E :: ADD :: E :: Nil)
    testLexer("e * e", E :: MULTIPLY :: E :: Nil)
    testLexer("e / e", E :: DIVIDE :: E :: Nil)
    testLexer("e ^ e", E :: POWER :: E :: Nil)
  }

  test("subtraction") {
    testLexer("e - 3.2", E :: SUBTRACT :: Decimal(3.2) :: Nil)
    testLexer(") - e", CLOSE_PAREN :: SUBTRACT :: E :: Nil)
    testLexer("x - e", Variable("x") :: SUBTRACT :: E :: Nil)
    testLexer("3 - e", Decimal(3) :: SUBTRACT :: E :: Nil)
  }

  test("negation") {
    testLexer("-e", NEGATE :: E :: Nil)
    testLexer("* - e", MULTIPLY :: NEGATE :: E :: Nil)
    testLexer("sin - e", SIN :: NEGATE :: E :: Nil)
    testLexer("( - e", OPEN_PAREN :: NEGATE :: E :: Nil)
  }

  // ------------------------------------------------------------
  // Functions
  // ------------------------------------------------------------

  test("parentheses") {
    testLexer("()", OPEN_PAREN :: CLOSE_PAREN :: Nil)
    testLexer("(e  )", OPEN_PAREN :: E :: CLOSE_PAREN :: Nil)
  }

  test("functions") {
    testLexer("sin(e)", SIN :: OPEN_PAREN :: E :: CLOSE_PAREN :: Nil)
    testLexer("cos(e)", COS :: OPEN_PAREN :: E :: CLOSE_PAREN :: Nil)
    testLexer("tan(e)", TAN :: OPEN_PAREN :: E :: CLOSE_PAREN :: Nil)
    testLexer("sqrt(e)", SQRT :: OPEN_PAREN :: E :: CLOSE_PAREN :: Nil)
    testLexer("log(e)", LOG :: OPEN_PAREN :: E :: CLOSE_PAREN :: Nil)
    testLexer("ln(e)", LN :: OPEN_PAREN :: E :: CLOSE_PAREN :: Nil)
  }

  // ------------------------------------------------------------
  // Expressions
  // ------------------------------------------------------------

  test("expression 1") {
    testLexer("(3 * 2) + 5",
      OPEN_PAREN :: Decimal(3) :: MULTIPLY :: Decimal(2) :: CLOSE_PAREN :: ADD :: Decimal(5) :: Nil
    )
  }

  test("expression 2") {
    testLexer("(e / sin(- pi))",
      OPEN_PAREN :: E :: DIVIDE :: SIN :: OPEN_PAREN :: NEGATE :: PI :: CLOSE_PAREN :: CLOSE_PAREN :: Nil
    )
  }

  test("expression 3") {
    testLexer("(- e - - pi) ^ ln(pi - - e)",
      OPEN_PAREN :: NEGATE :: E :: SUBTRACT :: NEGATE :: PI :: CLOSE_PAREN :: POWER :: LN :: OPEN_PAREN :: PI ::
        SUBTRACT :: NEGATE :: E :: CLOSE_PAREN :: Nil
    )
  }

  // ------------------------------------------------------------
  // Helper Methods
  // ------------------------------------------------------------

  def testLexer(in: String, exp: List[Token]): Unit = {
    val tokens = getTokens(new Lexer(in)).reverse
    val expected = END :: exp.reverse
    assert(tokens == expected.reverse)
  }

  def getTokens(lexer: Lexer, list: List[Token] = Nil): List[Token] = {
    val token = lexer.getNextToken
    val result = token :: list
    if (token != END) getTokens(lexer, result) else result
  }
}
