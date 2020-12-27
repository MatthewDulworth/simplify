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
  def parse(expression: String): Option[ASTree] = ???
}