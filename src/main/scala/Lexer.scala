class Lexer(val input: String) {

  private var cursor = -1
  private var previousToken: Option[Token] = None
  private var currentToken: Token = _

  def nextChar: Option[Char] = {
    cursor += 1
    if (cursor < input.length) Some(input.charAt(cursor)) else None
  }

  def getNextToken: Token = {
    val char = nextChar
    previousToken = Some(currentToken)
    currentToken = char match {
      case Some(c) => tokenFromChar(c)
      case None => EOF
    }
    currentToken
  }

  def tokenFromChar(char: Char): Token = char match {
    case '+' => ADD
    case '*' => MULTIPLY
    case '/' => DIVIDE
    case '^' => EXPONENT
    case '(' => OPEN_PAREN
    case ')' => CLOSE_PAREN
    case '-' => negOrSubToken
    case l if l.isLetter => letterToken
    case d if d.isDigit || d == '.' => numberToken(char)
    case w if w.isWhitespace => getNextToken
    case _ => InvalidToken(char)
  }

  def negOrSubToken: Token = previousToken match {
    case Some(NumberToken(n)) => SUBTRACT
    case Some(Variable(v)) => SUBTRACT
    case Some(CLOSE_PAREN) => SUBTRACT
    case _ => NEGATION
  }

  def numberToken(c: Char): Token = {
    val numBuilder = new StringBuilder(c.toString)
    var char = nextChar
    while (char.isDefined && (char.get.isDigit || char.get == '.')) {
      numBuilder.append(char.get)
      char = nextChar
    }
    val number = numBuilder.toString.toDoubleOption
    number match {
      case Some(d) => NumberToken(d)
      case None => InvalidToken('.')
    }
  }

  def letterToken: Token = {
    var c = nextChar
    val value = new StringBuilder
    while (c.isDefined && c.get.isLetter) {
      value.append(c.get)
      c = nextChar
    }

    value.toString() match {
      case "sin" => SIN
      case "cos" => COS
      case "tan" => TAN
      case _ => Variable(value.toString())
    }
  }
}