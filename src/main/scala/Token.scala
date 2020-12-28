// ------------------------------------------------------------
// Tokens
// ------------------------------------------------------------
sealed trait Token {
  val precedence: Int
  val symbol: String

  def hasHigherPrecedence(other: Token): Boolean = this.precedence > other.precedence
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

  override def hasHigherPrecedence(other: Token): Boolean = true
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

sealed trait Constant extends Number

case class Decimal(value: Double) extends Constant {
  override val symbol: String = value.toString
}

case object PI extends Constant {
  override val symbol: String = "pi"
}

case object E extends Constant {
  override val symbol: String = "e"
}

// ------------------------------------------------------------
// Unary Operators
// ------------------------------------------------------------
sealed trait Operator extends Token

sealed trait UnaryOperator extends Operator

case object NEGATE extends UnaryOperator {
  override val symbol: String = "neg"
  override val precedence: Int = 3
}

sealed trait Function extends UnaryOperator {
  override val precedence: Int = 4
}

case object SIN extends Function {
  override val symbol: String = "sin"
}

case object COS extends Function {
  override val symbol: String = "cos"
}

case object TAN extends Function {
  override val symbol: String = "tan"
}

case object SQRT extends Function {
  override val symbol: String = "sqrt"
}

case object LOG extends Function {
  override val symbol: String = "log"
}

case object LN extends Function {
  override val symbol: String = "ln"
}

// ------------------------------------------------------------
// Binary Operators
// ------------------------------------------------------------
sealed trait BinaryOperator extends Operator

sealed trait LeftAssoc extends BinaryOperator

sealed trait RightAssoc extends BinaryOperator

case object ADD extends LeftAssoc {
  override val precedence: Int = 1
  override val symbol: String = "+"
}

case object SUBTRACT extends LeftAssoc {
  override val precedence: Int = 1
  override val symbol: String = "-"
}

case object MULTIPLY extends LeftAssoc {
  override val precedence: Int = 2
  override val symbol: String = "*"
}

case object DIVIDE extends LeftAssoc {
  override val precedence: Int = 2
  override val symbol: String = "/"
}

case object POWER extends RightAssoc {
  override val precedence: Int = 3
  override val symbol: String = "^"
}