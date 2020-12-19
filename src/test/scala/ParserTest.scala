import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  def equalASTs(e: Option[ASTree], a: Option[ASTree]): Boolean = (e, a) match {
    case (_, None) => false
    case (None, _) => false
    case (None, None) => true
    case (e, a) => e.get.deepEquals(a.get)
  }

  test("Parser should parse simple binary operations") {

    val tree = Node(EmptyNode, ADD)
    tree.left = Node(tree, NumberToken(2))
    tree.right = Node(tree, NumberToken(3))
    val expected = Option(tree)

    val actual = Parser.parse("2 + 3")
    assert(equalASTs(expected, actual))

    actual match {
      case None => println("no tree")
      case _ => TreePrinter.print(actual.get)
    }
  }
}
