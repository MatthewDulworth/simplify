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
    newLeft match {
      case node: Node => node.parent = this
    }
    this
  }

  def setRight(newRight: Tree): Tree = {
    right = newRight
    newRight match {
      case node: Node => node.parent = this
    }
    this
  }
}