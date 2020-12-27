// ------------------------------------------------------------
// Tokens
// ------------------------------------------------------------
sealed trait Token {
  val precedence: Int
  val symbol: String
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
}

case object CLOSE_PAREN extends Token {
  override val precedence: Int = 0
  override val symbol: String = ")"
}

// ------------------------------------------------------------
// Numbers
// ------------------------------------------------------------
sealed trait Number extends Token {
  override val precedence: Int = 0
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

sealed trait UnaryOperator extends Operator {
  override val precedence: Int = 15
}

case object NEGATE extends UnaryOperator {
  override val symbol: String = "neg"
}

case object SIN extends UnaryOperator {
  override val symbol: String = "sin"
}

case object COS extends UnaryOperator {
  override val symbol: String = "cos"
}

case object TAN extends UnaryOperator {
  override val symbol: String = "tan"
}

case object SQRT extends UnaryOperator {
  override val symbol: String = "sqrt"
}

case object LOG extends UnaryOperator {
  override val symbol: String = "log"
}

case object LN extends UnaryOperator {
  override val symbol: String = "ln"
}

// ------------------------------------------------------------
// Binary Operators
// ------------------------------------------------------------
sealed trait BinaryOperator extends Operator

sealed trait LeftAssoc extends BinaryOperator

sealed trait RightAssoc extends BinaryOperator

case object ADD extends LeftAssoc {
  override val precedence: Int = 5
  override val symbol: String = "+"
}

case object SUBTRACT extends LeftAssoc {
  override val precedence: Int = 5
  override val symbol: String = "-"
}

case object MULTIPLY extends LeftAssoc {
  override val precedence: Int = 10
  override val symbol: String = "*"
}

case object DIVIDE extends LeftAssoc {
  override val precedence: Int = 10
  override val symbol: String = "/"
}

case object POWER extends RightAssoc {
  override val precedence: Int = 20
  override val symbol: String = "^"
}