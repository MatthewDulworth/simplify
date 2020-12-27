/**
 * Tokenizes strings representing mathematical expressions to make them ready for parsing by the Parser class.
 *
 * Ignores whitespace. Letters are treated as variables unless they match a support function name.
 *
 * @param expression The string to parse into tokens.
 */
class Lexer(val expression: String) {

  private val input = expression.replaceAll("\\s", "")
  private var cursor = -1
  private var previousToken: Option[Token] = None
  private var currentToken: Token = _

  /**
   * Finds the next token in the input string. The last token returned will be EOF.
   * Once one EOF is returned, all subsequent calls to getNExtToken will return EOF.
   *
   * @return The next token from the input.
   */
  def nextToken: Token = {
    val char = nextChar
    previousToken = Some(currentToken)
    currentToken = char match {
      case Some(c) => tokenFromChar(c)
      case None => END
    }
    currentToken
  }

  /**
   * @return THe next char in th input string.
   */
  private def nextChar: Option[Char] = {
    cursor += 1
    if (cursor < input.length) Some(input.charAt(cursor)) else None
  }

  /**
   * @param char The char to create a token from.
   * @return Token created from the given character.
   */
  private def tokenFromChar(char: Char): Token = char match {
    case '+' => ADD
    case '*' => MULTIPLY
    case '/' => DIVIDE
    case '^' => POWER
    case '(' => OPEN_PAREN
    case ')' => CLOSE_PAREN
    case '-' => negOrSubToken
    case l if l.isLetter => letterToken(char)
    case d if d.isDigit || d == '.' => numberToken(char)
    case _ => InvalidChar(char)
  }

  /**
   * Determines if the current '-' char represents subtraction or negation, returns the appropriate token.
   *
   * @return SUBTRACT or NEGATE.
   */
  private def negOrSubToken: Token = previousToken match {
    case Some(t: Number) => SUBTRACT
    case Some(CLOSE_PAREN) => SUBTRACT
    case _ => NEGATE
  }

  /**
   * Given an initial digit/decimal point of a number in the input, eats the subsequent digits
   * (or decimal points) of the number and returns a number token if the number is valid. Returns
   * an Invalid number if the number has too many/only consists of a decimal point.
   *
   * @param firstDigit The first digit (or decimal point) of the number.
   * @return A Number if the number format is valid, otherwise an InvalidNumber.
   */
  private def numberToken(firstDigit: Char): Token = {
    var char = Option(firstDigit)
    val numBuilder = new StringBuilder
    while (char.isDefined && (char.get.isDigit || char.get == '.')) {
      numBuilder.append(char.get)
      char = nextChar
    }
    cursor -= 1
    val numOption = numBuilder.toString.toDoubleOption
    numOption match {
      case Some(decimal) => Decimal(decimal)
      case None => InvalidNumber(numBuilder.toString)
    }
  }

  /**
   * Given the first letter of a function/variable name/named constant in the input eats the character
   * and all subsequent letters until it hits a non-letter character, then returns a function
   * token if one matches the name, otherwise returns a variable token.
   *
   * @param firstLetter The first letter of the function/variable name.
   * @return A function or variable token.
   */
  private def letterToken(firstLetter: Char): Token = {
    var char = Option(firstLetter)
    val value = new StringBuilder
    while (char.isDefined && char.get.isLetter) {
      value.append(char.get)
      char = nextChar
    }
    cursor -= 1
    value.toString() match {
      case "sqrt" => SQRT
      case "sin" => SIN
      case "cos" => COS
      case "tan" => TAN
      case "log" => LOG
      case "ln" => LN
      case "pi" => PI
      case "e" => E
      case _ => Variable(value.toString())
    }
  }
}