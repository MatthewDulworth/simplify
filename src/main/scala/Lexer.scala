class Lexer(val in: String) {

  private val input = in.replaceAll("\\s", "")
  private var cursor = -1
  private var previousToken: Option[Token] = None
  private var currentToken: Token = _

  def getNextToken: Token = {
    val char = nextChar
    previousToken = Some(currentToken)
    currentToken = char match {
      case Some(c) => tokenFromChar(c)
      case None => EOF
    }
    currentToken
  }

  private def nextChar: Option[Char] = {
    cursor += 1
    if (cursor < input.length) Some(input.charAt(cursor)) else None
  }

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
    case _ => InvalidToken(char)
  }

  private def negOrSubToken: Token = previousToken match {
    case Some(Number(_)) => SUBTRACT
    case Some(Variable(_)) => SUBTRACT
    case Some(CLOSE_PAREN) => SUBTRACT
    case _ => NEGATION
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
      case None => InvalidToken('.')
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