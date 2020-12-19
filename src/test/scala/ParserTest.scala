import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  def equalASTs(e: Option[ASTree], a: Option[ASTree]): Boolean = (e, a) match {
    case (_, None) => false
    case (None, _) => false
    case (None, None) => true
    case (e, a) => e.get.deepEquals(a.get)
    case (_, _) => false
  }

  def printAST(t: Option[ASTree]): Unit = t match {
    case None => println("Parse terminated with None")
    case _ => TreePrinter.print(t.get)
  }

  def testAST(input: String, expected: Option[ASTree], showInfo: Boolean): Unit = {
    val actual = Parser.parse(input)
    val testCondition = equalASTs(expected, actual)

    if (showInfo) {
      println("actual: ")
      printAST(actual)

      if (!testCondition) {
        println("expected: ")
        printAST(expected)
      }
    }

    assert(testCondition)
  }

  val showInfo = true

  test("Parser should parse simple binary operations") {

    val expected = Option({
      val tree = Node(EmptyNode, ADD)
      tree.left = Node(tree, NumberToken(2))
      tree.right = Node(tree, NumberToken(4))
      tree
    })
    testAST("2 + 3", expected, showInfo)
  }
}
