
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
  def eval(node: ASTree): Double = ???


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
  def simplify(node: ASTree): ASTree = ???
}
