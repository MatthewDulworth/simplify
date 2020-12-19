import org.scalatest.FunSuite
import scala.collection.mutable.ArrayBuffer

class LexerTest extends FunSuite {

  test("lexer should only produce an EOF token on an empty input") {
    val lexer = new Lexer("")
    val tokens = getTokens(lexer)
    assert(tokens.length == 1)
    assert(tokens(0) == EOF)
    for (i <- 1 to 10) {
      assert(lexer.getNextToken == EOF)
    }
  }

  test("lexer should tokenize numbers") {
    var lexer = new Lexer("1")
    assert(lexer.getNextToken == NumberToken(1))

    lexer = new Lexer("0.3")
    assert(lexer.getNextToken == NumberToken(0.3))

    lexer = new Lexer("345")
    assert(lexer.getNextToken == NumberToken(345))

    lexer = new Lexer("9879.991")
    assert(lexer.getNextToken == NumberToken(9879.991))
  }

  test("lexer should create invalid tokens invalid numbers") {
    var lexer = new Lexer(".")
    assert(lexer.getNextToken == InvalidToken('.'))

    lexer = new Lexer("3.245.5")
    assert(lexer.getNextToken == InvalidToken('.'))

    lexer = new Lexer(".....0")
    assert(lexer.getNextToken == InvalidToken('.'))
  }

  test("lexer should create tokens from binary operators") {
    var lexer = new Lexer("+")
    assert(lexer.getNextToken == ADD)

    lexer = new Lexer("*")
    assert(lexer.getNextToken == MULTIPLY)

    lexer = new Lexer("/")
    assert(lexer.getNextToken == DIVIDE)

    lexer = new Lexer("^")
    assert(lexer.getNextToken == EXPONENT)
  }

  test("lexer should correctly distinguish between negation and subtraction")(pending)

  test("lexer should ignore whitespace")(pending)

  test("lexer should tokenize variables")(pending)

  test("lexer should tokenize functions")(pending)

  test("lexer should tokenize complex expressions")(pending)

  test("lexer should create invalid tokens for invalid symbols")(pending)

  // helper methods
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
