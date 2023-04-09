package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GraphSpec extends AnyFlatSpec with should.Matchers {

    behavior of "UndirectedGraph"

    it should "create an empty graph" in {
        val target: UndirectedGraph[String, String] = UndirectedGraph[String, String]
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }

    it should "create a graph with one empty vertex" in {
        val graph: UndirectedGraph[String, String] = UndirectedGraph[String, String]
        val target = graph.addVertex("A")
        target.vertices shouldBe Set("A")
    }

    it should "create a graph with one edge" in {
        val graph: UndirectedGraph[String, Color] = UndirectedGraph[String, Color]
        val edge: UndirectedEdge[String, Color] = UndirectedEdge("A", "B", Color("red"))
        val target = graph.addEdge(edge)
        target.vertices.size shouldBe 2
    }

    it should "create an ordered graph with one edge" in {
        val graph: UndirectedGraph[String, Color] = UndirectedGraph.createOrdered[String, Color]
        val edge: UndirectedEdge[String, Color] = UndirectedEdge("A", "B", Color("red"))
        val target = graph.addEdge(edge)
        target.vertices shouldBe Set("A", "B")
        target.vertices.size shouldBe 2
    }

    behavior of "DirectedGraph"

    it should "create an empty graph" in {
        val target: DirectedGraph[String, String] = DirectedGraph[String, String]
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }

    it should "create a graph with one empty vertex" in {
        val graph: DirectedGraph[String, String] = DirectedGraph[String, String]
        val target = graph.addVertex("A")
        target.vertices shouldBe Set("A")
    }
}
