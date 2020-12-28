
/**
 * Abstract Syntax Tree. Represents an expression.
 */
sealed trait ASTree {
  val left: ASTree
  val right: ASTree
  val token: Token

  def simplify: ASTree
}

/**
 * Represents an empty expression. Used to terminate branches of a tree.
 */
case object Empty extends ASTree {
  override val left: ASTree = Empty
  override val right: ASTree = Empty
  override val token: Token = END

  override def simplify: ASTree = Empty
}

/**
 * Represents a non-empty expression.
 *
 * @param token The Number/Operation that the node is.
 * @param left  The left child of the node. Empty if a Number.
 * @param right The right child of the node. Empty if a Number or a UnaryOperator
 */
case class Node(token: Token, left: ASTree = Empty, right: ASTree = Empty) extends ASTree {

  /**
   * 0 × F simplifies to 0
   * F × 0 simplifies to 0
   * 1 × F simplifies to F
   * F × 1 simplifies to F
   * F^0 simplifies to 1
   * F^1 simplifies to F
   * (F a ) b simplifies to F ab
   * F a × F b simplifies to F a+b
   * F + 0 simplifies to F
   * 0 + F simplifies to F
   * 0/F simplifies to 0
   * An immediate application of e x to ln F (or of ln x to e F ) simplifies to F
   *
   * @return
   */
  def simplify: ASTree = token match {
    case _: Number => this
    case binOp: BinaryOperator => binOp.operation(left.simplify, right.simplify)
    case unOp: UnaryOperator => unOp.operation(left.simplify)
  }

  /**
   * @return A string representation of this expression.
   */
  override def toString: String = token match {
    case n: Number => n.symbol
    case b: BinaryOperator => "(" + left.toString + b.symbol + right.toString + ")"
    case u: UnaryOperator => u.symbol + "(" + left.toString + ")"
  }
}