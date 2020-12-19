import org.scalatest.FunSuite
import scala.collection.mutable.ArrayBuffer

class LexerTest extends FunSuite {

  test("lexer should produce only a single EOF token on an empty input") {
    val lexer = new Lexer("")
    val tokens = getTokens(lexer)
    assert(tokens.length == 1)
    assert(tokens(0) == EOF)
  }

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
