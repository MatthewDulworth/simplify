
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
  def getNextToken: Token = {
    val char = nextChar
    previousToken = Some(currentToken)
    currentToken = char match {
      case Some(c) => tokenFromChar(c)
      case None => EOF
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
    case '^' => EXPONENT
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
    case Some(Number(_)) => SUBTRACT
    case Some(Variable(_)) => SUBTRACT
    case Some(CLOSE_PAREN) => SUBTRACT
    case _ => NEGATE
  }

  private def numberToken(c: Char): Token = {
    var char = Option(c)
    val numBuilder = new StringBuilder
    while (char.isDefined && (char.get.isDigit || char.get == '.')) {
      numBuilder.append(char.get)
      char = nextChar
    }
    cursor -= 1
    val number = numBuilder.toString.toDoubleOption
    number match {
      case Some(d) => Number(d)
      case None => InvalidNumber(numBuilder.toString)
    }
  }

  private def letterToken(c: Char): Token = {
    var char = Option(c)
    val value = new StringBuilder
    while (char.isDefined && char.get.isLetter) {
      value.append(char.get)
      char = nextChar
    }
    cursor -= 1
    value.toString() match {
      case "sin" => SIN
      case "cos" => COS
      case "tan" => TAN
      case _ => Variable(value.toString())
    }
  }
}