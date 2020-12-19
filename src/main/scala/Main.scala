import scala.collection.mutable.ArrayBuffer

object Main extends App {

  @scala.annotation.tailrec
  def findTokens(lexer: Lexer, buffer: ArrayBuffer[Token]): Unit = {
    val t = lexer.getNextToken
    if (t != EOF) {
      buffer.addOne(t)
      findTokens(lexer, buffer)
    }
  }

  val lexer = new Lexer("x")
  val tokens = new ArrayBuffer[Token]
  findTokens(lexer, tokens)
  println(tokens)
}