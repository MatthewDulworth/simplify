sealed trait Tree {
  var parent: Tree
  var left: Tree
  var right: Tree
  val token: Token

}

case object EmptyNode extends Tree {
  var parent: Tree = EmptyNode
  var left: Tree = EmptyNode
  val right: Tree = EmptyNode
  val token: Token = EOF
}

case class Node(var parent: Tree, token: Token) extends Tree {
  var left: Tree = EmptyNode
  var right: Tree = EmptyNode

  def resetParent(): Tree = {
    parent = EmptyNode
    this
  }

  def setLeft(newLeft: Tree): Tree = {
    left = newLeft
    setChildParent(newLeft)
  }

  def setRight(newRight: Tree): Tree = {
    right = newRight
    setChildParent(newRight)
  }

  private def setChildParent(child: Tree) = child match {
    case node: Node => node.parent = this; this
    case _ => this
  }
}