import scala.collection.mutable.ArrayBuffer

object Main extends App {

  @scala.annotation.tailrec
  def findTokens(lexer: Lexer, buffer: ArrayBuffer[Token]): Unit = {
    val t = lexer.getNextToken
    buffer.addOne(t)
    if (t != EOF) {
      findTokens(lexer, buffer)
    }
  }

  val lexer = new Lexer("")
  val tokens = new ArrayBuffer[Token]
  findTokens(lexer, tokens)
  println(tokens)
}