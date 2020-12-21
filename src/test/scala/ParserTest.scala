import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("empty input") {
    testAST("", None)
  }

  test("simple binary operations") {
    var expected = Option({
      val tree = Node(EmptyNode, ADD)
      tree.left = Node(tree, Number(2))
      tree.right = Node(tree, Number(3))
      tree
    })
    testAST("2 + 3", expected)

    expected = Option({
      val tree = Node(EmptyNode, SUBTRACT)
      tree.left = Node(tree, Number(2))
      tree.right = Node(tree, Number(3))
      tree
    })
    testAST("2 - 3", expected)

    expected = Option({
      val tree = Node(EmptyNode, MULTIPLY)
      tree.left = Node(tree, Number(2))
      tree.right = Node(tree, Number(3))
      tree
    })
    testAST("2 * 3", expected)

    expected = Option({
      val tree = Node(EmptyNode, DIVIDE)
      tree.left = Node(tree, Number(2))
      tree.right = Node(tree, Number(3))
      tree
    })
    testAST("2 / 3", expected)
  }

  test("subtract a negative number") {
    val expected = Option({
      val tree = Node(EmptyNode, SUBTRACT)
      tree.setLeft(Node(tree, Number(2)))
      tree.setRight(Node(tree, NEGATION))
      tree.right.setRight(Node(tree.right, Number(3)))
      tree
    })
    testAST("2 - -3", expected)
  }

  test("simple parentheses") {
    val expected = Option({
      val tree = Node(EmptyNode, MULTIPLY)
      tree.setLeft(Node(tree, Number(3)))
      tree.setRight(Node(tree, ADD))
      tree.right.setLeft(Node(tree.right, Number(2)))
      tree.right.setRight(Node(tree.right, Number(1)))
      tree
    })
    testAST("3 * (2 + 1)", expected)
  }

  test("negative exponents") {
    val expected = Option({
      val tree = Node(EmptyNode, EXPONENT)
      tree.setLeft(Node(tree, Number(2)))
      tree.setRight(Node(tree, NEGATION))
      tree.right.setRight(Node(tree.right, Number(3)))
      tree
    })
    testAST("2 ^ -3", expected)
  }

  ignore("variables and numbers") {
    val expected = Option({
      val tree = Node(EmptyNode, EXPONENT)
      tree.setLeft(Node(tree, Number(2)))
      tree.setRight(Node(tree, NEGATION))
      tree.right.setRight(Node(tree.right, Number(3)))
      tree
    })
    testAST("2 ^ x ", expected)
  }

  ignore("complex expression") {
    val expected = Option({
      val tree = Node(EmptyNode, EXPONENT)
      tree.setLeft(Node(tree, Number(2)))
      tree.setRight(Node(tree, NEGATION))
      tree.right.setRight(Node(tree.right, Number(3)))
      tree
    })
    testAST("2 ^ x ", expected)
  }

  // -------------------------------------------------------------
  // Helper Methods
  // -------------------------------------------------------------
  def equalASTs(e: Option[ASTree], a: Option[ASTree]): Boolean = (e, a) match {
    case (Some(_), None) => false
    case (None, Some(_)) => false
    case (None, None) => true
    case (e, a) => e.get.deepEquals(a.get)
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

}
