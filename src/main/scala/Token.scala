// ------------------------------------------------------------
// Tokens
// ------------------------------------------------------------
sealed trait Token {
  val precedence: Int
  val symbol: String

  def >(other: Token): Boolean = this.precedence > other.precedence
}

case object END extends Token {
  override val precedence: Int = -1
  override val symbol: String = "end"
}

sealed trait InvalidToken extends Token {
  override val precedence: Int = -1
  val message: String
}

case class InvalidChar(char: Char) extends InvalidToken {
  override val symbol: String = char.toString
  override val message: String = s"Invalid character encountered: $symbol"
}

case class InvalidNumber(symbol: String) extends InvalidToken {
  override val message: String = s"Invalid number encountered: $symbol"
}

// ------------------------------------------------------------
// Parentheses
// ------------------------------------------------------------
case object OPEN_PAREN extends Token {
  override val precedence: Int = 0
  override val symbol: String = "("

  override def >(other: Token): Boolean = true
}

case object CLOSE_PAREN extends Token {
  override val precedence: Int = 0
  override val symbol: String = ")"
}

// ------------------------------------------------------------
// Numbers
// ------------------------------------------------------------
sealed trait Number extends Token {
  override val precedence: Int = 5
}

case class Variable(symbol: String) extends Number

sealed trait Constant extends Number {
  val value: Double
}

case class Decimal(value: Double) extends Constant {
  override val symbol: String = value.toString
}

case object PI extends Constant {
  override val value: Double = math.Pi
  override val symbol: String = "pi"
}

case object E extends Constant {
  override val value: Double = math.E
  override val symbol: String = "e"
}

// ------------------------------------------------------------
// Unary Operators
// ------------------------------------------------------------
sealed trait Operator extends Token

sealed trait UnaryOperator extends Operator {
  def operation(simplify: ASTree): ASTree
}

case object NEGATE extends UnaryOperator {
  override val symbol: String = "neg"
  override val precedence: Int = 3

  override def >(other: Token): Boolean = {
    this.precedence >= other.precedence
  }

  override def operation(simplify: ASTree): ASTree = ???
}

sealed trait Function extends UnaryOperator {
  override val precedence: Int = 4
}

case object SIN extends Function {
  override val symbol: String = "sin"

  override def operation(left: ASTree): ASTree = left.token match {
    case PI => Node(Decimal(0))
    case c: Constant => Node(Decimal(math.sin(c.value)))
    case _ => Node(SIN, left)
  }
}

case object COS extends Function {
  override val symbol: String = "cos"

  override def operation(left: ASTree): ASTree = left.token match {
    case Decimal(a) => Node(Decimal(math.cos(a)))
    case _ => Node(COS, left)
  }
}

case object TAN extends Function {
  override val symbol: String = "tan"

  override def operation(left: ASTree): ASTree = left.token match {
    case Decimal(a) => Node(Decimal(math.tan(a)))
    case _ => Node(TAN, left)
  }

}

case object SQRT extends Function {
  override val symbol: String = "sqrt"

  // sqrt(x ^ y) = x ^ y -2  if y is a power of 2
  override def operation(left: ASTree): ASTree = left.token match {
    case POWER if isPowerOf2(left.right.token) => Node(POWER, left.left, newValOf(left.right)).simplify
    case Decimal(a) => Node(Decimal(math.sqrt(a)))
    case _ => Node(SQRT, left)
  }

  private def newValOf(node: ASTree): ASTree = {
    val value = node.token.asInstanceOf[Decimal].value
    Node(DIVIDE, Node(Decimal(value)), Node(Decimal(2)))
  }

  private def isPowerOf2(token: Token): Boolean = token match {
    case Decimal(a) if (a == math.floor(a) && a != 0) => (a.toInt & (a.toInt - 1)) == 0
    case _ => false
  }
}

case object LOG extends Function {
  override val symbol: String = "log"

  // log(1) = 0
  // log(10) = 1
  // log(10 ^ x) = x
  override def operation(left: ASTree): ASTree = left.token match {
    case Decimal(1) => Node(Decimal(0))
    case Decimal(10) => Node(Decimal(1))
    case POWER if left.left.token == Decimal(10) => left.right
    case Decimal(a) => Node(Decimal(math.log10(a)))
    case _ => Node(LOG, left)
  }
}

case object LN extends Function {
  override val symbol: String = "ln"

  // ln(1) = 0
  // ln(e) = 1
  // ln(e ^ x) = x
  override def operation(left: ASTree): ASTree = left.token match {
    case Decimal(1) => Node(Decimal(0))
    case E => Node(Decimal(1))
    case POWER if left.left.token == E => left.right
    case Decimal(a) => Node(Decimal(math.log(a)))
    case _ => Node(LN, left)
  }
}

// ------------------------------------------------------------
// Binary Operators
// ------------------------------------------------------------
sealed trait BinaryOperator extends Operator {
  def operation(left: ASTree, right: ASTree): ASTree
}

sealed trait LeftAssoc extends BinaryOperator

sealed trait RightAssoc extends BinaryOperator {
  override def >(other: Token): Boolean = {
    this.precedence >= other.precedence
  }
}

case object ADD extends LeftAssoc {
  override val precedence: Int = 1
  override val symbol: String = "+"

  // x + 0 => x
  // 0 + x => x
  // x + x => 2x
  override def operation(left: ASTree, right: ASTree): ASTree = (left.token, right.token) match {
    case (Decimal(0), _) => right
    case (_, Decimal(0)) => left
    case (Decimal(a), Decimal(b)) => Node(Decimal(a + b))
    case (x, y) if x == y => Node(MULTIPLY, Node(Decimal(2)), right)
    case (_, _) => Node(ADD, left, right)
  }
}

case object SUBTRACT extends LeftAssoc {
  override val precedence: Int = 1
  override val symbol: String = "-"

  // x - 0 => x
  // 0 - x => neg(x)
  // x - x => 0
  override def operation(left: ASTree, right: ASTree): ASTree = (left.token, right.token) match {
    case (_, Decimal(0)) => left
    case (Decimal(0), _) => Node(NEGATE, right)
    case (x, y) if x == y => Node(Decimal(0))
    case (Decimal(a), Decimal(b)) => if (b > a) Node(NEGATE, Node(Decimal(math.abs(a - b)))) else Node(Decimal(a - b))
    case (_, _) => Node(SUBTRACT, left, right)
  }
}

case object MULTIPLY extends LeftAssoc {
  override val precedence: Int = 2
  override val symbol: String = "*"

  // 0 * x => 0
  // x * 0 => 0
  // x * 1 => x
  // 1 * x => x
  // x * x => x ^ 2
  override def operation(left: ASTree, right: ASTree): ASTree = (left.token, right.token) match {
    case (_, Decimal(0)) => Node(Decimal(0))
    case (Decimal(0), _) => Node(Decimal(0))
    case (_, Decimal(1)) => left
    case (Decimal(1), _) => right
    case (Decimal(a), Decimal(b)) => Node(Decimal(a * b))
    case (x, y) if x == y => Node(POWER, left, Node(Decimal(2)))
    case (_, _) => Node(MULTIPLY, left, right)
  }
}

case object DIVIDE extends LeftAssoc {
  override val precedence: Int = 2
  override val symbol: String = "/"

  // 0 / x = 0
  // x / x = 1
  // x / 1 = x
  override def operation(left: ASTree, right: ASTree): ASTree = (left.token, right.token) match {
    case (Decimal(0), _) => Node(Decimal(0))
    case (x, y) if x == y => Node(Decimal(1))
    case (_, Decimal(1)) => left
    case (Decimal(a), Decimal(b)) => Node(Decimal(a / b))
    case (_, _) => Node(DIVIDE, left, right)
  }
}

case object POWER extends RightAssoc {
  override val precedence: Int = 3
  override val symbol: String = "^"

  // 0 ^ x => 0
  // 1 ^ x => 1
  // x ^ 0 => 1
  // x ^ 1 => x
  // e ^ ln(x) => x
  // 10 ^ log(x) => x
  override def operation(left: ASTree, right: ASTree): ASTree = (left.token, right.token) match {
    case (Decimal(0), _) => Node(Decimal(0))
    case (Decimal(1), _) => Node(Decimal(1))
    case (_, Decimal(0)) => Node(Decimal(1))
    case (_, Decimal(1)) => left
    case (E, LN) => right.left
    case (Decimal(10), LOG) => right.left
    case (Decimal(a), Decimal(b)) => Node(Decimal(math.pow(a, b)))
    case (_, _) => Node(POWER, left, right)
  }
}