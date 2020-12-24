import TreePrinter.PrintableNode

sealed trait ASTree extends PrintableNode {
  var parent: ASTree
  var left: ASTree
  var right: ASTree
  val token: Token

  def setLeft(newLeft: ASTree): Unit = {}

  def setRight(newLeft: ASTree): Unit = {}

  def deepEquals(other: ASTree): Boolean
}

case object EmptyNode extends ASTree {
  var parent: ASTree = EmptyNode
  var left: ASTree = EmptyNode
  var right: ASTree = EmptyNode
  val token: Token = EOF

  override def getLeft: PrintableNode = null

  override def getRight: PrintableNode = null

  override def getText: String = null

  override def deepEquals(other: ASTree): Boolean = other match {
    case EmptyNode => true
    case _ => false
  }

  override def toString: String = ""
}

case class Node(var parent: ASTree, token: Token) extends ASTree {
  var left: ASTree = EmptyNode
  var right: ASTree = EmptyNode


  def resetParent(): ASTree = {
    parent = EmptyNode
    this
  }

  override def setLeft(newLeft: ASTree): Unit = {
    left = newLeft
    setChildParent(newLeft)
  }

  override def setRight(newRight: ASTree): Unit = {
    right = newRight
    setChildParent(newRight)
  }

  private def setChildParent(child: ASTree): Unit = child match {
    case node: Node => node.parent = this
    case _ =>
  }

  override def toString: String = token match {
    case Number(value) => value.toString
    case Variable(name) => name
    case ADD => "(" + left.toString + "+" + right.toString + ")"
    case SUBTRACT => "(" + left.toString + "-" + right.toString + ")"
    case MULTIPLY => "(" + left.toString + "*" + right.toString + ")"
    case DIVIDE => "(" + left.toString + "/" + right.toString + ")"
    case EXPONENT => "(" + left.toString + "^" + right.toString + ")"
    case NEGATE => "(-" + right.toString + ")"
    case SIN => "sin(" + right.toString + ")"
    case COS => "cos(" + right.toString + ")"
    case TAN => "tan(" + right.toString + ")"
  }

  // debug methods
  override def getLeft: PrintableNode = left match {
    case EmptyNode => null
    case n: Node => left
  }

  override def getRight: PrintableNode = right match {
    case EmptyNode => null
    case n: Node => right
  }

  override def getText: String = token.symbol

  override def deepEquals(other: ASTree): Boolean = other match {
    case node: Node => token == node.token && left.deepEquals(other.left) && right.deepEquals(other.right)
    case _ => false
  }
}

class Root extends Node(EmptyNode, EOF);