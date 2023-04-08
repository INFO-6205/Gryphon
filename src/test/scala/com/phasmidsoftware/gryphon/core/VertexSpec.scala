package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class VertexSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Vertex"

    it should "attribute" in {
        Vertex.empty("A").attribute shouldBe "A"
    }

    it should "degree" in {
        Vertex.empty("A").degree shouldBe 0
        Vertex.empty("A").addEdge(DirectedEdge("A", "B", "ab")).degree shouldBe 1
    }

    it should "adjacent" in {
        Vertex.empty("A").adjacent shouldBe AdjacencyList.empty
        Vertex.empty("A").addEdge(DirectedEdge("A", "B", "ab")).adjacent shouldBe AdjacencyList(Seq(DirectedEdge("A", "B", "ab")))
    }

    it should "addEdge" in {
        val a = Vertex.empty[String, DirectedEdge[String, String]]("A")
        a.addEdge(DirectedEdge("A", "B", "ab")) shouldBe new ConcreteVertex("A", AdjacencyList(Seq(DirectedEdge("A", "B", "ab")))) // leave "new" intact.
    }

}
