
/**
 * Represents a single item in a mathematical expression.
 */
sealed trait Token {
  val symbol: String
  val precedence: Int

  def isLessThan(other: Token): Boolean = precedence <= other.precedence
}

/**
 * Represents the end of the expression.
 */
case object EOF extends Token {
  override val symbol: String = ""
  override val precedence: Int = Int.MinValue
}

// -------------------------------------------------------
// Numbers & Variables
// -------------------------------------------------------

case class Number(value: Double) extends Token {
  override val symbol: String = value.toString
  override val precedence: Int = Int.MaxValue
}

case class Variable(value: String) extends Token {
  override val symbol: String = value
  override val precedence: Int = Int.MaxValue
}


// -------------------------------------------------------
// Parentheses
// -------------------------------------------------------

case object OPEN_PAREN extends Token {
  override val symbol = "("
  override val precedence = 1
}

case object CLOSE_PAREN extends RightAssociative {
  override val symbol = ")"
  override val precedence = 1
}


// -------------------------------------------------------
// Functions
// -------------------------------------------------------

sealed trait Function extends Token {
  override val precedence: Int = Int.MaxValue
  val operation: Double => Double
}

case object SIN extends Function {
  override val symbol: String = "sin"
  val operation: Double => Double = Math.sin
}

case object COS extends Function {
  override val symbol: String = "cos"
  val operation: Double => Double = Math.cos
}

case object TAN extends Function {
  override val symbol: String = "tan"
  val operation: Double => Double = Math.tan
}


// -------------------------------------------------------
// Operators
// -------------------------------------------------------

/**
 * overrides isLessThan for right associative operators
 */
trait RightAssociative extends Token {
  override def isLessThan(other: Token): Boolean = precedence < other.precedence
}

case object NEGATE extends RightAssociative {
  override val symbol = "neg"
  override val precedence = 4
  val operation: Double => Double = _ * -1
}

/**
 * Holds the operation that binary operators use.
 */
sealed trait BinaryOperator extends Token {
  val operation: (Double, Double) => Double
}

case object ADD extends BinaryOperator {
  override val symbol = "+"
  override val precedence = 2
  override val operation: (Double, Double) => Double = _ + _
}

case object SUBTRACT extends BinaryOperator {
  override val symbol = "-"
  override val precedence = 2
  override val operation: (Double, Double) => Double = _ - _
}

case object MULTIPLY extends BinaryOperator {
  override val symbol = "*"
  override val precedence = 3
  override val operation: (Double, Double) => Double = _ * _
}

case object DIVIDE extends BinaryOperator {
  override val symbol = "/"
  override val precedence = 3
  override val operation: (Double, Double) => Double = _ / _
}

case object EXPONENT extends BinaryOperator with RightAssociative {
  override val symbol = "^"
  override val precedence = 5
  override val operation: (Double, Double) => Double = Math.pow
}


// -------------------------------------------------------
// Invalid Tokens
// -------------------------------------------------------

/**
 * Created by the lexer whenever some invalid number or character is encountered.
 */
sealed trait InvalidToken extends Token {
  override val precedence: Int = Int.MinValue
}

case class InvalidNumber(invalidNum: String) extends InvalidToken {
  override val symbol: String = invalidNum
}

case class InvalidChar(invalidChar: Char) extends InvalidToken {
  override val symbol: String = invalidChar.toString
}