import scala.annotation.tailrec

/**
 * Utility object to parse mathematical expressions into abstract syntax trees.
 */
case object Parser {

  /**
   * Parses strings representing mathematical expressions into abstract syntax trees (ASTree).
   *
   * @param expression The expression to parse.
   * @return An option containing the root of the ASTree if it can be generated.
   */
  def parse(expression: String): Option[ASTree] = {
    val lexer = new Lexer(expression)
    val currentNode = Node(EmptyNode, lexer.getNextToken)
    currentNode.token match {
      case EOF => None
      case _ => buildTree(lexer, currentNode)
    }
  }

  /**
   * Builds the syntax tree from the expression.
   *
   * @param lexer     The lexer attached to the expression to be parsed.
   * @param startNode The current node.
   * @return An option containing the root of the ASTree if it can be generated.
   */
  @tailrec private def buildTree(lexer: Lexer, startNode: ASTree): Option[ASTree] = {
    val token = lexer.getNextToken

    token match {
      case EOF => treeRoot(startNode)
      case t: InvalidToken => None
      case _ =>
        var currentNode = traverseCurrentUp(startNode, token)
        currentNode = token match {
          case CLOSE_PAREN => closeParens(currentNode)
          case _ => insertToken(currentNode, token)
        }
        buildTree(lexer, currentNode)
    }
  }

  /**
   * Traverses up the tree to find the node to make the parent of the given token.
   *
   * @param startNode The current location in the tree.
   * @param token     The token to consider.
   * @return The new current node.
   */
  private def traverseCurrentUp(startNode: ASTree, token: Token): ASTree = token match {
    case OPEN_PAREN | NEGATE => startNode
    case _ =>
      var currentNode = startNode
      while (currentNode.parent != EmptyNode && token.isLessThan(currentNode.token)) {
        currentNode = currentNode.parent
      }
      currentNode
  }

  /**
   * Closes a set of parentheses. Finds the first open paren above the current node and
   * removes it from the tree. Returns the right child of the removed node.
   *
   * @param openParenNode The current location in the tree.
   * @return The right child of the removed
   */
  private def closeParens(openParenNode: ASTree): ASTree = openParenNode.parent match {
    case parent: Node =>
      parent.setRight(openParenNode.right)
      parent
    case EmptyNode =>
      openParenNode.right.asInstanceOf[Node].resetParent()
      openParenNode.right
  }

  /**
   * Inserts the given token and returns the inserted node.
   *
   * @param currentNode The current location in the tree.
   * @param token       The token to insert.
   * @return The inserted node.
   */
  private def insertToken(currentNode: ASTree, token: Token): ASTree = token match {
    case OPEN_PAREN | NEGATE => insert(token, currentNode, currentNode.right)
    case token if token.isLessThan(currentNode.token) => insert(token, EmptyNode, currentNode)
    case _ => insert(token, currentNode, currentNode.right)
  }

  /**
   * Inserts and returns a new node constructed from the given token into the syntax tree as
   * the right child of the given parent node. Sets the given child node to be its left child.
   *
   * @param token  The token to insert into the tree.
   * @param parent The node to be the parent of the token.
   * @param child  The node to be the left child of the token.
   * @return The inserted node.
   */
  private def insert(token: Token, parent: ASTree, child: ASTree): ASTree = {
    val newNode = Node(parent, token)
    newNode.setLeft(child)
    parent.setRight(newNode)
    newNode
  }

  /**
   * Finds the root of a syntax tree.
   *
   * @param startNode A node of an ASTree.
   * @return The root of the ASTree the given node is a member of.
   */
  private def treeRoot(startNode: ASTree): Option[ASTree] = {
    var node = startNode
    while (node.parent != EmptyNode) {
      node = node.parent
    }
    Some(node)
  }
}