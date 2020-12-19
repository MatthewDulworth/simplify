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
      case InvalidToken(_) => None
      case _ =>
        var currentNode = traverseCurrentUp(node, token)
        currentNode = addNode(currentNode, token)
        buildTree(lexer, currentNode)
    }
  }

  def traverseCurrentUp(startNode: Tree, token: Token): Tree = token match {
    case OPEN_PAREN | NEGATION => startNode
    case _ =>
      var currentNode = startNode
      while (currentNode.parent != EmptyNode && token.isLessThan(currentNode.token)) {
        currentNode = currentNode.parent
      }
      currentNode
  }

  def addNode(node: Tree, token: Token): Tree = token match {
    case CLOSE_PAREN => closeParens(node)
    case _ => insertNewNode(node, token)
  }

  def closeParens(node: Tree): Tree = node.parent match {
    case parent: Node => parent.setRight(node.right)
    case EmptyNode => node.right.asInstanceOf[Node].resetParent()
  }

  def insertNewNode(currentNode: Tree, token: Token): Tree = token match {
    case token if token.isLessThan(currentNode.token) =>
      assert(currentNode.parent == EmptyNode)
      val newNode = Node(EmptyNode, token)
      newNode.setLeft(currentNode)
    case _ =>
      val newNode = Node(currentNode, token)
      newNode.setLeft(currentNode.right)
  }

  def treeRoot(startNode: Tree): Option[Tree] = {
    var node = startNode
    while (node.parent != EmptyNode) {
      node = node.parent
    }
    Some(node)
  }
}