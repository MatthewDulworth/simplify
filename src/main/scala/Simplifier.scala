
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
}
