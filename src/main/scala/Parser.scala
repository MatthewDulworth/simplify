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
        var currentNode = updateCurrentNode(node, token)
        currentNode = insertNode(currentNode, token)
        buildTree(lexer, currentNode)
    }
  }

  def updateCurrentNode(currentNode: Tree, token: Token): Tree = token match {
    case r if r.rightAssociative => traverseUp(currentNode, token, _ > _)
    case t if t != OPEN_PAREN && t != CLOSE_PAREN => traverseUp(currentNode, token, _ >= _)
  }

  def traverseUp(currentNode: Tree, token: Token, operator: (Double, Double) => Boolean): Tree = {
    var node = currentNode
    while (node.parent != EmptyNode && operator(node.token.precedence, token.precedence)) {
      node = node.parent
    }
    node
  }

  def insertNode(tree: Tree, token: Token): Tree = ???

  def treeRoot(node: Tree): Option[Tree] = ???
}












