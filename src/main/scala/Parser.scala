import scala.annotation.tailrec

case object Parser {

  def parse(input: String): Option[Tree] = {
    val lexer = new Lexer(String)
    val currentNode = Node(EmptyNode, lexer.getNextToken)
    buildTree(lexer, currentNode)
  }

  @tailrec
  private def buildTree(lexer: Lexer, node: Tree): Option[Tree] = {
    val token = lexer.getNextToken

    token match {
      case EOF => treeRoot(node)
      case i: InvalidToken => None
      case _ =>
        var currentNode = traverseCurrentUp(node, token)
        currentNode = addNode(currentNode, token)
        buildTree(lexer, currentNode)
    }
  }

  def traverseCurrentUp(currentNode: Tree, token: Token): Tree = token match {
    case OPEN_PAREN | NEGATION => currentNode
    case r if r.rightAssociative => doTraversalUp(currentNode, token, _ > _)
    case _ => doTraversalUp(currentNode, token, _ >= _)
  }

  def doTraversalUp(currentNode: Tree, token: Token, condition: (Double, Double) => Boolean): Tree = {
    var node = currentNode
    while (node.parent != EmptyNode && condition(node.token.precedence, token.precedence)) {
      node = node.parent
    }
    node
  }

  def addNode(node: Tree, token: Token): Tree = token match {
    case CLOSE_PAREN => closeParens(node)
    case _ => insertNewNode(node, token)
  }

  def closeParens(node: Tree): Tree = node.parent match {
    case parent: Node => parent.setRight(node.right)
    case EmptyNode => node.right.asInstanceOf[Node].resetParent()
  }

  def insertNewNode(node: Tree, token: Token): Tree = ???


  def treeRoot(node: Tree): Option[Tree] = ???
}












