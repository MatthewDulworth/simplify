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
    case OPEN_PAREN => ???
    case CLOSE_PAREN => ???
    case END => shuntRemaining(yard)
    case token: InvalidToken => None
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
      val right :: left :: _ = expressions
      val expression = Node(operator, left, right)
      ShuntYard(operators.tail, expression :: expressions.tail.tail)
    }

    @tailrec def pushOp(operator: Operator, yard: ShuntYard = this): ShuntYard = {
      if (operator.hasHigherPrecedence(yard.operators.head))
        ShuntYard(operator :: yard.operators, yard.expressions)
      else
        pushOp(operator, yard.pushOpToExpr())
    }
  }

}