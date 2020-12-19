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

  def testAST(input: String, expected: Option[ASTree]): Unit = {
    val actual = Parser.parse(input)
    val testCondition = equalASTs(expected, actual)

    if (!testCondition) {
      println("actual: ")
      printAST(actual)
      println("expected: ")
      printAST(expected)
    }

    assert(testCondition)
  }

  test("simple binary operations") {
    var expected = Option({
      val tree = Node(EmptyNode, ADD)
      tree.left = Node(tree, NumberToken(2))
      tree.right = Node(tree, NumberToken(3))
      tree
    })
    testAST("2 + 3", expected)

    expected = Option({
      val tree = Node(EmptyNode, SUBTRACT)
      tree.left = Node(tree, NumberToken(2))
      tree.right = Node(tree, NumberToken(3))
      tree
    })
    testAST("2 - 3", expected)

    expected = Option({
      val tree = Node(EmptyNode, MULTIPLY)
      tree.left = Node(tree, NumberToken(2))
      tree.right = Node(tree, NumberToken(3))
      tree
    })
    testAST("2 * 3", expected)

    expected = Option({
      val tree = Node(EmptyNode, DIVIDE)
      tree.left = Node(tree, NumberToken(2))
      tree.right = Node(tree, NumberToken(3))
      tree
    })
    testAST("2 / 3", expected)
  }

  test("subtract a negative number") {
    val expected = Option({
      val tree = Node(EmptyNode, SUBTRACT)
      tree.setLeft(Node(tree, NumberToken(2)))
      tree.setRight(Node(tree, NEGATION))
      tree.right.setRight(Node(tree, NumberToken(3)))
      tree
    })
    testAST("2 - -3", expected)
  }

  test("negative exponents") {
    val expected = Option({
      val tree = Node(EmptyNode, EXPONENT)
      tree.setLeft(Node(tree, NumberToken(2)))
      tree.setRight(Node(tree, NEGATION))
      tree.right.setRight(Node(tree.right, NumberToken(3)))
      tree
    })
    testAST("2 ^ -3", expected)
  }
}
