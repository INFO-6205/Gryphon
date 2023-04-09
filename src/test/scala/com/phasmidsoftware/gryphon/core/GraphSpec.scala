package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GraphSpec extends AnyFlatSpec with should.Matchers {

    private val red: Color = Color("red")
    private val vertexA = "A"
    private val vertexB = "B"

    behavior of "UndirectedGraph"

    it should "create an empty graph" in {
        val target: UndirectedGraph[String, String] = UndirectedGraph[String, String]
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }

    it should "create a graph with one empty vertex" in {
        val graph: UndirectedGraph[String, String] = UndirectedGraph[String, String]
        val target = graph.addVertex(vertexA)
        target.vertices shouldBe Set(vertexA)
    }

    it should "create a graph with one edge" in {
        val graph: UndirectedGraph[String, Color] = UndirectedGraph[String, Color]
        val edge: UndirectedEdge[String, Color] = UndirectedEdge(vertexA, vertexB, red)
        val target = graph.addEdge(edge)
        target.vertices.size shouldBe 2
    }

    it should "create an ordered graph with one edge" in {
        val graph: UndirectedGraph[String, Color] = UndirectedGraph.createOrdered[String, Color]
        val edge: UndirectedEdge[String, Color] = UndirectedEdge(vertexA, vertexB, red)
        val target = graph.addEdge(edge)
        target.vertices shouldBe Set(vertexA, vertexB)
        target.vertices.size shouldBe 2
        target.edgeAttributes.headOption shouldBe Some(red)
    }

    behavior of "DirectedGraph"

    it should "create an empty graph" in {
        val target: DirectedGraph[String, String] = DirectedGraph[String, String]
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }

    it should "create a graph with one empty vertex" in {
        val graph: DirectedGraph[String, String] = DirectedGraph[String, String]
        val target = graph.addVertex(vertexA)
        target.vertices shouldBe Set(vertexA)
    }
}
