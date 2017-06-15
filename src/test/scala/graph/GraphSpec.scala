package graph

import graph.node.GNode
import org.scalatest.{FlatSpec, Matchers}

class GraphSpec extends FlatSpec with Matchers {

  behavior of "The walkGraph"

  it should "return a single node for single node graph" in new Fixture {
    val nodes = graph.walkGraph(commonNode)
    nodes should not be empty
    nodes should have size (1)
    nodes.head should equal (commonNode)
  }

  it should "return a list of all nodes in graph" in new Fixture {
    val nodes = graph.walkGraph(rootNode)
    nodes.toSet should equal (nodeSet)
  }

  it should "return a list of nodes fall under given arbitrary node" in new Fixture {
    val nodes = graph.walkGraph(node12)
    nodes should have size (4)
    nodes.toSet should equal (Set(node12, commonNode, node121, node122))
  }

  it should "not return any duplicate nodes in the list" in new Fixture {
    val nodes = graph.walkGraph(rootNode)
    nodes should contain (commonNode)
    nodes.size should equal (nodeSet.size)
  }

  behavior of "The paths"

  it should "return a list with single path for a single node graph" in new Fixture {
    val paths = graph.paths(commonNode)
    paths should have size (1)
    paths.head should equal (List(commonNode))
  }

  it should "return all paths for a given graph" in new Fixture {
    val paths = graph.paths(rootNode)
    paths should have size (6)
    paths.toSet should equal (
      Set(
        List(rootNode, node11, node111),
        List(rootNode, node11, node112),
        List(rootNode, node11, commonNode),
        List(rootNode, node12, node121),
        List(rootNode, node12, node122),
        List(rootNode, node12, commonNode)
      )
    )
  }

  it should "return all paths starting from an arbitrary node" in new Fixture {
    val paths = graph.paths(node12)
    paths should have size (3)
    paths.toSet should equal (
      Set(
        List(node12, node121),
        List(node12, node122),
        List(node12, commonNode)
      )
    )
  }

  trait Fixture {
    val graph = new Graph()
    val commonNode = GNode("aaa", Nil)
    val node111 = GNode("111", Nil)
    val node112 = GNode("112", Nil)
    val node121 = GNode("121", Nil)
    val node122 = GNode("122", Nil)
    val node11 = GNode("11", List(node111, node112, commonNode))
    val node12 = GNode("12", List(commonNode, node121, node122))
    val rootNode = GNode("1", List(node11, node12))
    val nodeSet = Set(rootNode, node11, node12, commonNode, node111, node112, node121, node122)
  }
}
