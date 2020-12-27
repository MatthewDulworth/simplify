import org.scalatest.FunSuite

class LexerTest extends FunSuite {

  test("empty input") {
    val exp = List(END)
    testLexer("", exp)
  }

  test("numbers")(pending)

  test("invalid numbers")(pending)

  test("binary operators")(pending)

  test("parentheses")(pending)

  test("parentheses expressions")(pending)

  test("ignore whitespace")(pending)

  test("negation vs subtraction")(pending)

  test("variables vs functions")(pending)

  test("complex expression")(pending)

  def testLexer(in: String, exp: List[Token]): Unit = {
    val tokens = getTokens(new Lexer(in)).reverse
    assert(tokens == exp)
  }

  def getTokens(lexer: Lexer, list: List[Token] = Nil): List[Token] = {
    val token = lexer.getNextToken
    val result = token :: list
    if (token != END) getTokens(lexer, result) else result
  }
}
