package graph

import graph.node.GNode

class Graph {

  def walkGraph(node: GNode): List[GNode] = {
    def walkNodes(nodes: Seq[GNode]): List[GNode] =
      nodes.foldLeft(List.empty[GNode]) { (acc, node) =>
        node :: acc ++ walkNodes(node.getChildren)
      }
    walkNodes(List(node)).distinct
  }

  def paths(node: GNode): List[List[GNode]] = {
    def nodePaths(nodes: List[GNode]): List[List[GNode]] =
      for {
        node <- nodes
        childPaths = nodePaths(node.getChildren)
        nodePath <- if (childPaths.isEmpty) List(Nil) else childPaths
      } yield node :: nodePath
    nodePaths(List(node))
  }
}

object GraphApp extends App {
  val graph = new Graph

  println("Test application...")

  val singleNode = GNode("aaa", Nil)
  val graphNode = GNode("1", List(
      GNode("11", List(
        GNode("111", Nil),
        GNode("112", Nil),
        singleNode
      )),
      GNode("12", List(
        singleNode,
        GNode("121", Nil),
        GNode("122", Nil)
      )))
    )

  val nodes = graph.walkGraph(graphNode)
  val paths = graph.paths(graphNode)

  println(nodes)
  println(paths)
}
