import scala.annotation.tailrec

case object Parser {

  def parse(input: String): Option[ASTree] = {
    val lexer = new Lexer(input)
    val currentNode = Node(EmptyNode, lexer.getNextToken)
    buildTree(lexer, currentNode)
  }

  @tailrec private def buildTree(lexer: Lexer, startNode: ASTree): Option[ASTree] = {
    val token = lexer.getNextToken

    token match {
      case EOF => treeRoot(startNode)
      case InvalidToken(_) => None
      case _ =>
        var currentNode = traverseCurrentUp(startNode, token)
        currentNode = token match {
          case CLOSE_PAREN => closeParens(currentNode)
          case _ => insertNewNode(currentNode, token)
        }
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

  private def closeParens(node: ASTree): ASTree = node.parent match {
    case parent: Node =>
      parent.setRight(node.right)
      parent
    case EmptyNode =>
      node.right.asInstanceOf[Node].resetParent()
      node
  }

  private def insertNewNode(currentNode: ASTree, token: Token): ASTree = token match {
    case OPEN_PAREN | NEGATION => insert(token, currentNode, currentNode.right)
    case token if token.isLessThan(currentNode.token) => insert(token, EmptyNode, currentNode)
    case _ => insert(token, currentNode, currentNode.right)
  }

  private def insert(token: Token, parent: ASTree, child: ASTree): ASTree = {
    val newNode = Node(parent, token)
    newNode.setLeft(child)
    parent.setRight(newNode)
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