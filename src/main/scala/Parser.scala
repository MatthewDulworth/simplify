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
   * @return An option containing the root of the ASTree if it can be generated.
   */
  def parse(expression: String): Option[ASTree] = {
    val lexer = new Lexer(expression)
    shunt(lexer)
  }

  @tailrec private def shunt(lexer: Lexer, yard: ShuntYard = ShuntYard()): Option[ASTree] = lexer.nextToken match {
    case number: Number => shunt(lexer, yard.pushExpr(number))
    case operator: Operator => shunt(lexer, yard.pushOp(operator))
    case OPEN_PAREN => shunt(lexer, yard.pushOp(OPEN_PAREN))
    case CLOSE_PAREN => shunt(lexer, yard.closeParens())
    case END => shuntRemaining(yard)
    case _: InvalidToken => None
  }

  @tailrec private def shuntRemaining(yard: ShuntYard): Option[ASTree] = {
    if (yard.operators.head == END)
      yard.expressions.headOption
    else
      shuntRemaining(yard.pushOpToExpr())
  }

  private final case class ShuntYard(operators: List[Token] = List(END), expressions: List[ASTree] = List()) {

    def pushExpr(token: Token): ShuntYard = ShuntYard(operators, Node(token) :: expressions)

    // pre: operators.size >= 1, expressions.size >= 2
    def pushOpToExpr(): ShuntYard = {
      val operator = operators.head

      val (left, right, tail) = operator match {
        case _: BinaryOperator => (expressions.tail.head, expressions.head, expressions.tail.tail)
        case _: UnaryOperator => (expressions.head, Empty, expressions.tail)
      }

      val expression = Node(operator, left, right)
      ShuntYard(operators.tail, expression :: tail)
    }

    @tailrec def pushOp(operator: Token, yard: ShuntYard = this): ShuntYard = {
      if (operator > yard.operators.head)
        ShuntYard(operator :: yard.operators, yard.expressions)
      else
        pushOp(operator, yard.pushOpToExpr())
    }

    @tailrec def closeParens(yard: ShuntYard = this): ShuntYard = {
      if (yard.operators.head == OPEN_PAREN)
        ShuntYard(yard.operators.tail, yard.expressions)
      else
        closeParens(yard.pushOpToExpr())
    }
  }

}