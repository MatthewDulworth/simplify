sealed trait Token {
  val symbol: String
  val precedence: Int
  val rightAssociative = false
}

case class InvalidToken(invalidChar: Char) extends Token {
  override val symbol: String = invalidChar.toString
  override val precedence: Int = -1
}

case object EOF extends Token {
  override val symbol: String = ""
  override val precedence: Int = -1
}

case class NumberToken(value: Double) extends Token {
  override val symbol: String = value.toString
  override val precedence: Int = Int.MaxValue
}

case class Variable(value: String) extends Token {
  override val symbol: String = value
  override val precedence: Int = Int.MaxValue
}

case object OPEN_PAREN extends Token {
  override val symbol = "("
  override val precedence = 1
}

case object CLOSE_PAREN extends Token {
  override val symbol = ")"
  override val precedence = 1
  override val rightAssociative = true;
}

case object NEGATION extends Token {
  override val symbol = "neg"
  override val precedence = 4
  val operation: Double => Double = _ * -1
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
// Binary Operators
// -------------------------------------------------------
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

case object EXPONENT extends BinaryOperator {
  override val symbol = "^"
  override val precedence = 5
  override val rightAssociative = true
  override val operation: (Double, Double) => Double = Math.pow
}