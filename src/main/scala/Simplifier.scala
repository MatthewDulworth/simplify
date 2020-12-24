
/**
 * Utility object to simplify Abstract Syntax Trees.
 */
case object Simplifier {

  /**
   * Naive evaluation of an ASTree. Evaluates variables to 1.
   *
   * @param node
   * @return
   */
  def eval(node: ASTree): Double = node.token match {
    case binOp: BinaryOperator => binOp.operation(eval(node.left), eval(node.right))
    case func: Function => func.operation(eval(node.right))
    case NEGATE => NEGATE.operation(eval(node.right))
    case Number(x) => x
    case Variable(_) => 1
    case _ => 0
  }


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
   * @param node
   * @return
   */
  def simplify(node: ASTree): ASTree = node.token match {
    case Number(_) => Node(EmptyNode, node.token)
    case Variable(_) => Node(EmptyNode, node.token)
    case NEGATE => negate(simplify(node.right))
    case SIN => sin(simplify(node.right))
    case COS => cos(simplify(node.right))
    case TAN => tan(simplify(node.right))
    case ADD => add(simplify(node.left), simplify(node.right))
    case SUBTRACT => subtract(simplify(node.left), simplify(node.right))
    case MULTIPLY => multiply(simplify(node.left), simplify(node.right))
    case DIVIDE => divide(simplify(node.left), simplify(node.right))
    case EXPONENT => multiply(simplify(node.left), simplify(node.right))
    case _ => EmptyNode
  }

  def add(left: ASTree, right: ASTree): ASTree = (left.token, right.token) match {
    case (Number(a), Number(b)) => Node(EmptyNode, Number(a + b))
    case (_, 0) => left
    case (0, _) => right
    case (_, _) => ???
  }

  def subtract(left: ASTree, right: ASTree): ASTree = ???

  def multiply(left: ASTree, right: ASTree): ASTree = ???

  def divide(left: ASTree, right: ASTree): ASTree = ???

  def exponentiate(left: ASTree, right: ASTree): ASTree = ???

  def negate(right: ASTree): ASTree = ???

  def sin(right: ASTree): ASTree = ???

  def cos(right: ASTree): ASTree = ???

  def tan(right: ASTree): ASTree = ???

  def tree(root: Token, left: Token, right: Token): ASTree = {
    val node = Node(EmptyNode, root)
    node.setLeft(Node(node, left))
    node.setRight(Node(node, right))
    node
  }
}
