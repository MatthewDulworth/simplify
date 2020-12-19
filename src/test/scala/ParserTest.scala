import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("Parser should parse simple binary operations") {
    val AST = Parser.parse("2 + 3")

    AST match {
      case None => println("no tree")
      case _ => TreePrinter.print(AST.get)
    }
  }
}
