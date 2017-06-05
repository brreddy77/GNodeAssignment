package graph.node

trait GNode {
  def getName: String
  def getChildren: List[GNode]

  override def toString: String = getName
}

object GNode {
  def apply(name: String, children: List[GNode]): GNode = new GNode {
    val getName = name
    val getChildren = children
  }
}