import scala.annotation.tailrec

/**
 * Utility object to parse mathematical expressions into abstract syntax trees.
 */
case object Parser {

  /**
   * Parses strings representing mathematical expressions into abstract syntax trees
   * using the Shunting Yard Algorithm for parsing expressions.
   *
   * @param expression The expression to parse.
   * @return An option containing the root of the ASTree if one can be generated.
   */
  def parse(expression: String): Option[ASTree] = {
    val lexer = new Lexer(expression)
    shunt(lexer)
  }

  /**
   * Performs the Shunting Yard Algorithm on the expression defined by the given lexer.
   *
   * @param lexer A lexer representing a mathematical expression.
   * @param yard  An object containing the expression and operator stacks used in the Shunting Yard Algorithm
   * @return An ASTree representing the expression if one can be generated, None otherwise.
   */
  @tailrec private def shunt(lexer: Lexer, yard: ShuntYard = ShuntYard()): Option[ASTree] = lexer.nextToken match {
    case number: Number => shunt(lexer, yard.pushExpr(number))
    case operator: Operator => shunt(lexer, yard.pushOp(operator))
    case OPEN_PAREN => shunt(lexer, yard.pushOp(OPEN_PAREN))
    case CLOSE_PAREN => shunt(lexer, yard.closeParens())
    case END => shuntRemainingOps(yard)
    case _: InvalidToken => None
  }

  /**
   * Shunts all operations in the given yard to expressions.
   *
   * @param yard The yard to shunt.
   * @return An option containing the root of the syntax tree.
   */
  @tailrec private def shuntRemainingOps(yard: ShuntYard): Option[ASTree] = {
    if (yard.operators.head == END)
      yard.expressions.headOption
    else
      shuntRemainingOps(yard.pushOpToExpr())
  }

  /**
   * Represents the state of the shunting yard algorithm stacks at a single step in the algorithm.
   * Contains the functions to move to the next step of the algorithm.
   *
   * @param operators   The stack of operators in the shunting yard.
   * @param expressions The stack of expressions in the shunting yard.
   */
  private final case class ShuntYard(operators: List[Token] = List(END), expressions: List[ASTree] = List()) {

    /**
     * Creates an expression from the given token and pushes it to the expression stack.
     *
     * @param token The token to push.
     * @return A ShuntYard with the expression pushed.
     */
    def pushExpr(token: Token): ShuntYard = ShuntYard(operators, Node(token) :: expressions)

    /**
     * Pops the top operation and converts it to an expression on the expression stack.
     *
     * pre: operators.size >= 1, expressions.size >= 2
     *
     * @return A ShuntYard with the top operation converted to an expression.
     */
    def pushOpToExpr(): ShuntYard = {
      val operator = operators.head

      val (left, right, tail) = operator match {
        case _: BinaryOperator => (expressions.tail.head, expressions.head, expressions.tail.tail)
        case _: UnaryOperator => (expressions.head, Empty, expressions.tail)
      }

      val expression = Node(operator, left, right)
      ShuntYard(operators.tail, expression :: tail)
    }

    /**
     * Converts all operators with precedence greater than the given operator to expressions on the expression stack
     * and then pushes the given operator to the operator stack.
     *
     * @param operator The operator to push.
     * @param yard     An intermediate ShuntingYard used in recursion. Leave as default argument.
     * @return A ShuntYard with the operator pushed.
     */
    @tailrec def pushOp(operator: Token, yard: ShuntYard = this): ShuntYard = {
      if (operator > yard.operators.head)
        ShuntYard(operator :: yard.operators, yard.expressions)
      else
        pushOp(operator, yard.pushOpToExpr())
    }

    /**
     * Closes the last open parenthesis and pushes its contents as an expression to the expression stack.
     *
     * @param yard An intermediate ShuntingYard used in recursion. Leave as default argument.
     * @return A ShuntYard with the last open parenthesis closed.
     */
    @tailrec def closeParens(yard: ShuntYard = this): ShuntYard = {
      if (yard.operators.head == OPEN_PAREN)
        ShuntYard(yard.operators.tail, yard.expressions)
      else
        closeParens(yard.pushOpToExpr())
    }
  }

}