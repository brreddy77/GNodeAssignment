package graph

import graph.node.GNode

import scala.annotation.tailrec

class Graph {

  def walkGraph(node: GNode): List[GNode] = {
    def walkNodes(nodes: List[GNode]): List[GNode] =
      nodes.foldLeft(List.empty[GNode]) { (acc, node) =>
        node :: acc ++ walkNodes(node.getChildren)
      }
    walkNodes(List(node)).distinct
  }

  def walkGraphTailRecursive(node: GNode): List[GNode] = {
    @tailrec
    def walkNodes(nodes: Set[GNode], acc: Set[GNode]): List[GNode] = {
      val collected = acc ++ nodes
      val remaining = nodes.flatMap(_.getChildren).diff(collected)
      if (remaining.isEmpty)
        collected.toList
      else
        walkNodes(remaining, collected)
    }

    walkNodes(Set(node), Set.empty[GNode])
  }

  // Alternative walkGraph with a mutable collection
  def walkGraphWithMutableCollection(node: GNode): List[GNode] = {
    val visited = scala.collection.mutable.Set[GNode]()
    def walkNodes(nodes: List[GNode]): Unit =
      nodes.foreach { node =>
        if (!visited.contains(node)) {
          visited.add(node)
          walkNodes(node.getChildren)
        }
      }
    walkNodes(List(node))
    visited.toList
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

  def pathsTailRecursive(node: GNode): List[List[GNode]] = {
    @tailrec
    def nodePaths(paths: List[List[GNode]]): List[List[GNode]] = {
      val nextPaths = for {
        path <- paths
        tailNode = path.last
        child <- tailNode.getChildren
      } yield path :+ child
      if (nextPaths.isEmpty)
        paths
      else
        nodePaths(nextPaths)
    }
    nodePaths(List(List(node)))
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
  val altNodes = graph.walkGraph(graphNode)
  val paths = graph.paths(graphNode)

  println(nodes)
  println(altNodes)
  println(paths)
}
