package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GraphSpec extends AnyFlatSpec with should.Matchers {

    private val red: Color = Color("red")
    private val vertexA = "A"
    private val vertexB = "B"

    behavior of "UndirectedGraph"

    it should "create an empty graph" in {
        val target: UndirectedGraph[String, String] = UndirectedGraph[String, String]("empty")
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }

    it should "create a graph with one empty vertex" in {
        val graph: UndirectedGraph[String, String] = UndirectedGraph[String, String]("test")
        val target = graph.addVertex(vertexA)
        target.vertices shouldBe Set(vertexA)
    }

    it should "create a graph with one edge" in {
        val graph: UndirectedGraph[String, Color] = UndirectedGraph[String, Color]("test")
        val edge: UndirectedEdge[String, Color] = UndirectedEdge(vertexA, vertexB, red)
        val target = graph.addEdge(edge)
        target.vertices.size shouldBe 2
    }

    it should "create an ordered graph with one edge" in {
        val graph: UndirectedGraph[String, Color] = UndirectedGraph.createOrdered[String, Color]("test")
        val edge: UndirectedEdge[String, Color] = UndirectedEdge(vertexA, vertexB, red)
        val target = graph.addEdge(edge)
        target.vertices shouldBe Set(vertexA, vertexB)
        target.vertices.size shouldBe 2
        target.edgeAttributes.headOption shouldBe Some(red)
    }

    it should "dfs" in {
        import Journal._
        val graph = UndirectedGraph[String, Int]("test")
        val target = graph.addEdge(UndirectedEdge("A", "B", 1)).addEdge(UndirectedEdge("B", "C", 2))
        val visitor = Visitor.createPostQueue[String]
        target.dfs(visitor)("A") match {
            case v: IterableVisitor[String, _] => v.iterator.toSeq shouldBe Seq("C", "B", "A")
        }
    }

    behavior of "DirectedGraph"

    it should "create an empty graph" in {
        val target: DirectedGraph[String, String] = DirectedGraph[String, String]("test")
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }

    it should "create a graph with one empty vertex" in {
        val graph: DirectedGraph[String, String] = DirectedGraph[String, String]("test")
        val target = graph.addVertex(vertexA)
        target.vertices shouldBe Set(vertexA)
    }

    it should "dfs pre-order" in {
        import Journal._
        val graph = DirectedGraph[String, Int]("test")
        val target = graph.addEdge(DirectedEdge("A", "B", 1)).addEdge(DirectedEdge("B", "C", 2))
        val visitor = Visitor.createPreQueue[String]
        target.dfs(visitor)("A") match {
            case v: IterableVisitor[String, _] => v.iterator.toSeq shouldBe Seq("A", "B", "C")
        }
    }

    it should "dfs reverse post-order" in {
        import Journal._
        val graph = DirectedGraph[String, Int]("test")
        val target = graph.addEdge(DirectedEdge("A", "B", 1)).addEdge(DirectedEdge("B", "C", 2))
        val visitor = Visitor.reversePostList[String]
        target.dfs(visitor)("A") match {
            case v: IterableVisitor[String, _] => v.iterator.toSeq shouldBe Seq("A", "B", "C")
        }
    }
}
