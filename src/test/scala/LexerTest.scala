import org.scalatest.FunSuite

class LexerTest extends FunSuite {

  test("empty input") {
    testLexer("", Nil)
  }

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

  test("binary operators")(pending)

  test("parentheses")(pending)

  test("parentheses expressions")(pending)

  test("ignore whitespace")(pending)

  test("negation vs subtraction")(pending)

  test("variables vs functions")(pending)

  test("complex expression")(pending)

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
