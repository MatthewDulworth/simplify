import scala.annotation.tailrec

case object Parser {

  def parse(input: String): Option[ASTree] = {
    val lexer = new Lexer(input)
    val currentNode = Node(EmptyNode, lexer.getNextToken)
    buildTree(lexer, currentNode)
  }

  @tailrec private def buildTree(lexer: Lexer, node: ASTree): Option[ASTree] = {
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

  private def traverseCurrentUp(startNode: ASTree, token: Token): ASTree = token match {
    case OPEN_PAREN | NEGATION => startNode
    case _ =>
      var currentNode = startNode
      while (currentNode.parent != EmptyNode && token.isLessThan(currentNode.token)) {
        currentNode = currentNode.parent
      }
      currentNode
  }

  private def addNode(node: ASTree, token: Token): ASTree = token match {
    case CLOSE_PAREN => closeParens(node)
    case _ => insertNewNode(node, token)
  }

  private def closeParens(node: ASTree): ASTree = node.parent match {
    case parent: Node =>
      parent.setRight(node.right)
      parent
    case EmptyNode =>
      node.right.asInstanceOf[Node].resetParent()
      node
  }

  private def insertNewNode(currentNode: ASTree, token: Token): ASTree = token match {
    case token if token.isLessThan(currentNode.token) =>
      assert(currentNode.parent == EmptyNode)
      val newNode = Node(EmptyNode, token)
      newNode.setLeft(currentNode)
      newNode
    case _ =>
      val newNode = Node(currentNode, token)
      newNode.setLeft(currentNode.right)
      currentNode.right = newNode
      newNode
  }

  private def treeRoot(startNode: ASTree): Option[ASTree] = {
    var node = startNode
    while (node.parent != EmptyNode) {
      node = node.parent
    }
    Some(node)
  }
}