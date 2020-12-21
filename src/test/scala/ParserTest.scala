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
      tree.setRight(Node(tree, NEGATE))
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
      tree.setRight(Node(tree, NEGATE))
      tree.right.setRight(Node(tree.right, Number(3)))
      tree
    })
    testAST("2 ^ -3", expected)
  }

  test("variables and functions") {
    val expected = Option({
      val tree = Node(EmptyNode, ADD)
      tree.setLeft(Node(tree, EXPONENT))
      tree.left.setLeft(Node(tree.left, Number(2)))
      tree.left.setRight(Node(tree.left, Variable("x")))
      tree.setRight(Node(tree, TAN))
      tree.right.setRight(Node(tree.right, Number(4.56)))
      tree
    })
    testAST("2 ^ x + tan(4.56)", expected)
  }

  test("complex expression") {
    val expected = Option({
      val tree = Node(EmptyNode, ADD)

      tree.setLeft(Node(tree, MULTIPLY))
      tree.left.setLeft(Node(tree.left, Variable("ye")))
      tree.left.setRight(Node(tree.left, Number(3)))

      tree.setRight(Node(tree, DIVIDE))
      tree.right.setLeft(Node(tree.right, EXPONENT))
      tree.right.left.setLeft(Node(tree.right.left, Variable("x")))
      tree.right.left.setRight(Node(tree.right.left, Number(55.34)))

      tree.right.setRight(Node(tree.right, SIN))
      val subTree = Node(tree.right.right, SUBTRACT)
      subTree.setLeft(Node(subTree, ADD))
      subTree.left.setLeft(Node(subTree.left, Number(3)))
      subTree.left.setRight(Node(subTree.left, DIVIDE))
      subTree.left.right.setLeft(Node(subTree.left.right, Number(4)))
      subTree.left.right.setRight(Node(subTree.left.right, Number(5)))
      subTree.setRight(Node(subTree, Number(2)))
      tree.right.right.setRight(subTree)

      tree
    })
    testAST("ye * 3 + x ^ 55.34 / sin(3 + 4 / 5 - 2)", expected)
  }

  test("more negatives") {
    val expected = Option({
      val tree = Node(EmptyNode, MULTIPLY)
      tree.setLeft(Node(tree, NEGATE))
      tree.left.setRight(Node(tree.left, Variable("x")))

      tree.setRight(Node(tree, NEGATE))
      val subTree = Node(tree.right, EXPONENT)
      subTree.setLeft(Node(subTree, Variable("x")))
      subTree.setRight(Node(subTree, NEGATE))
      subTree.right.setRight(Node(subTree.right, Variable("x")))
      tree.right.setRight(subTree)
      tree
    })
    testAST("-x * -x ^ -x", expected)
  }

  test("still more negatives") {
    val expected = Option({
      val tree = Node(EmptyNode, DIVIDE)

      tree.setLeft(Node(tree, DIVIDE))
      tree.left.setLeft(Node(tree, MULTIPLY))
      tree.left.left.setLeft(Node(tree.left.left, NEGATE))
      tree.left.left.left.setRight(Node(tree.left.left.left, Variable("x")))

      tree.left.setRight(Node(tree.left, NEGATE))
      var subTree = Node(tree.left.right, SUBTRACT)
      subTree.setLeft(Node(subTree, Variable("x")))
      subTree.setRight(Node(subTree, NEGATE))
      subTree.right.setRight(Node(subTree.right, Variable("x")))
      tree.left.right.setRight(subTree)

      tree.left.left.setRight(Node(tree.left, NEGATE))
      subTree = Node(tree.left.left.right, EXPONENT)
      subTree.setLeft(Node(subTree, Variable("x")))
      subTree.setRight(Node(subTree, NEGATE))
      subTree.right.setRight(Node(subTree.right, Variable("x")))
      tree.left.left.right.setRight(subTree)

      tree.setRight(Node(tree, TAN))
      tree.right.setRight(Node(tree.right, NEGATE))
      tree.right.right.setRight(Node(tree.right.right, Variable("x")))

      tree
    })
    testAST("(- x * - x ^ -x) / - (x - -x) / tan(-x)", expected)
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
