import TreePrinter.PrintableNode

sealed trait ASTree extends PrintableNode {
  var parent: ASTree
  var left: ASTree
  var right: ASTree
  val token: Token

  def setLeft(newLeft: ASTree): Unit = {}

  def setRight(newLeft: ASTree): Unit = {}

  def deepEquals(other: ASTree): Boolean

  override def toString: String = token.toString
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

  // debug methods
  override def getLeft: PrintableNode = left match {
    case EmptyNode => null
    case Node(_, _) => left
  }

  override def getRight: PrintableNode = right match {
    case EmptyNode => null
    case Node(_, _) => right
  }

  override def getText: String = token.symbol

  override def deepEquals(other: ASTree): Boolean = other match {
    case node: Node => token == node.token && left.deepEquals(other.left) && right.deepEquals(other.right)
    case _ => false
  }
}