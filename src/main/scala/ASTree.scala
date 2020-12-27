sealed trait ASTree {
  val left: ASTree
  val right: ASTree
  val token: Token
}

case object Empty extends ASTree {
  override val left: ASTree = Empty
  override val right: ASTree = Empty
  override val token: Token = END
}

case class Node(token: Token, left: ASTree = Empty, right: ASTree = Empty) extends ASTree {
  override def toString: String = token match {
    case n: Number => n.symbol
    case b: BinaryOperator => "(" + left.toString + b.symbol + right.toString + ")"
    case u: UnaryOperator => u.symbol + "(" + right.toString + ")"
  }
}