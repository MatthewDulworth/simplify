
/**
 * Abstract Syntax Tree. Represents an expression.
 */
sealed trait ASTree {
  val left: ASTree
  val right: ASTree
  val token: Token
}

/**
 * Represents an empty expression. Used to terminate branches of a tree.
 */
case object Empty extends ASTree {
  override val left: ASTree = Empty
  override val right: ASTree = Empty
  override val token: Token = END
}

/**
 * Represents a non-empty expression.
 *
 * @param token The Number/Operation that the node is.
 * @param left  The left child of the node. Empty if a Number.
 * @param right The right child of the node. Empty if a Number or a UnaryOperator
 */
case class Node(token: Token, left: ASTree = Empty, right: ASTree = Empty) extends ASTree {
  override def toString: String = token match {
    case n: Number => n.symbol
    case b: BinaryOperator => "(" + left.toString + b.symbol + right.toString + ")"
    case u: UnaryOperator => u.symbol + "(" + left.toString + ")"
  }
}