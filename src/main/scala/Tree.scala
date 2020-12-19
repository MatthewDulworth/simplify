sealed trait Tree {
  var parent: Tree
  val token: Token
}

case class Node(var parent: Tree, token: Token) extends Tree {
  var left: Tree = EmptyNode
  var right: Tree = EmptyNode

  def setLeft(newLeft: Tree): Unit = {
    left = newLeft
    newLeft match {
      case node: Node => node.parent = this
    }
  }

  def setRight(newRight: Tree): Unit = {
    right = newRight
    newRight match {
      case node: Node => node.parent = this
    }
  }
}

case object EmptyNode extends Tree {
  var parent: Tree = EmptyNode
  val token: Token = EOF
}