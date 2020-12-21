import org.scalatest.FunSuite
import scala.collection.mutable.ArrayBuffer

class LexerTest extends FunSuite {

  test("empty input") {
    val lexer = new Lexer("")
    val tokens = getTokens(lexer)
    assert(tokens.length == 1)
    assert(tokens(0) == EOF)
    for (i <- 1 to 10) {
      assert(lexer.getNextToken == EOF)
    }
  }

  test("numbers") {
    var lexer = new Lexer("1")
    assert(lexer.getNextToken == Number(1))

    lexer = new Lexer("0.3")
    assert(lexer.getNextToken == Number(0.3))

    lexer = new Lexer("345")
    assert(lexer.getNextToken == Number(345))

    lexer = new Lexer("9879.991")
    assert(lexer.getNextToken == Number(9879.991))
  }

  test("invalid numbers") {
    var lexer = new Lexer(".")
    assert(lexer.getNextToken == InvalidNumber("."))

    lexer = new Lexer("3.245.5")
    assert(lexer.getNextToken == InvalidNumber("3.245.5"))

    lexer = new Lexer(".....0")
    assert(lexer.getNextToken == InvalidNumber(".....0"))
  }

  test("binary operators") {
    var lexer = new Lexer("+")
    assert(lexer.getNextToken == ADD)

    lexer = new Lexer("*")
    assert(lexer.getNextToken == MULTIPLY)

    lexer = new Lexer("/")
    assert(lexer.getNextToken == DIVIDE)

    lexer = new Lexer("^")
    assert(lexer.getNextToken == EXPONENT)
  }

  test("parentheses") {
    val lexer = new Lexer("()");
    val tokens = getTokens(lexer)
    val exp = Vector(OPEN_PAREN, CLOSE_PAREN, EOF)
    assert(tokens == exp)
  }

  test("parentheses expressions") {
    val lexer = new Lexer("3*(2+1)")
    val tokens = getTokens(lexer)
    val exp = Vector(Number(3), MULTIPLY, OPEN_PAREN, Number(2), ADD, Number(1), CLOSE_PAREN, EOF)
    assert(tokens == exp)
  }

  test("ignore whitespace") {
    testLexer("3. 0   0 3  +  2       ^    5", Vector(
      Number(3.003), ADD, Number(2), EXPONENT, Number(5)
    ))
  }

  test("negation vs subtraction") {
    testLexer("(-2) - -3", Vector(
      OPEN_PAREN, NEGATION, Number(2), CLOSE_PAREN, SUBTRACT, NEGATION, Number(3)
    ))
  }

  test("variables vs functions") {
    testLexer("cos(3) + x / y", Vector(
      COS, OPEN_PAREN, Number(3), CLOSE_PAREN, ADD, Variable("x"), DIVIDE, Variable("y")
    ))
  }

  test("complex expression") {
    testLexer("2 + (3 ^ 4) / cos(20 * x^y) + 2", Vector(
      Number(2), ADD, OPEN_PAREN, Number(3), EXPONENT, Number(4), CLOSE_PAREN, DIVIDE, COS, OPEN_PAREN,
      Number(20), MULTIPLY, Variable("x"), EXPONENT, Variable("y"), CLOSE_PAREN, ADD, Number(2)
    ))
  }

  test("invalid characters") {
    testLexer("$%}", Vector(
      InvalidChar('$'), InvalidChar('%'), InvalidChar('}')
    ))
  }

  // helper methods
  def testLexer(input: String, expected: Vector[Token]): Unit = {
    val lexer = new Lexer(input)
    val tokens = getTokens(lexer)
    assert(tokens == expected :+ EOF)
  }

  def getTokens(lexer: Lexer): Vector[Token] = {
    val buffer = new ArrayBuffer[Token]
    findTokens(lexer, buffer)
    buffer.toVector
  }

  def findTokens(lexer: Lexer, buffer: ArrayBuffer[Token]): Unit = {
    val t = lexer.getNextToken
    buffer.addOne(t)
    if (t != EOF) {
      findTokens(lexer, buffer)
    }
  }
}
