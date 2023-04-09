package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class VertexMapSpec extends AnyFlatSpec with should.Matchers {

    private val vertexA = "A"
    private val vertexB = "B"

    behavior of "OrderedVertexMap"

    it should "keys" in {
        val target: VertexMap[String, DirectedEdge[String, Int]] = OrderedVertexMap.empty
        target.keys shouldBe Set.empty
    }

    it should "values" in {
        val target: VertexMap[String, DirectedEdge[String, Int]] = OrderedVertexMap.empty
        target.values.isEmpty shouldBe true
    }

    it should "addEdge" in {
        val target: VertexMap[String, DirectedEdge[String, Int]] = OrderedVertexMap.empty
        val edge: DirectedEdge[String, Int] = DirectedEdge(vertexA, vertexB, 42)
        val targetUpdated = target.addEdge(vertexA, edge)
        targetUpdated.keys shouldBe Set(vertexA)
        targetUpdated.edges shouldBe Seq(edge)
    }

    it should "addVertex" in {
        val target: VertexMap[String, DirectedEdge[String, Int]] = OrderedVertexMap.empty
        val targetUpdated = target.addVertex(vertexA)
        targetUpdated.keys shouldBe Set(vertexA)
        targetUpdated.edges.isEmpty shouldBe true
    }

    behavior of "UnorderedVertexMap"

    private val red: Color = Color("red")
    private val blue: Color = Color("blue")

    it should "keys" in {
        val target: VertexMap[Color, DirectedEdge[Color, Int]] = UnorderedVertexMap.empty
        target.keys shouldBe Set.empty
    }

    it should "values" in {
        val target: VertexMap[Color, DirectedEdge[Color, Int]] = UnorderedVertexMap.empty
        target.values.isEmpty shouldBe true
    }

    it should "addEdge" in {
        val target: VertexMap[Color, DirectedEdge[Color, Int]] = UnorderedVertexMap.empty
        val edge: DirectedEdge[Color, Int] = DirectedEdge(red, blue, 42)
        val targetUpdated = target.addEdge(red, edge)
        targetUpdated.keys shouldBe Set(red)
        targetUpdated.edges shouldBe Seq(edge)
    }

    it should "addVertex" in {
        val target: VertexMap[Color, DirectedEdge[Color, Int]] = UnorderedVertexMap.empty
        val targetUpdated = target.addVertex(red)
        targetUpdated.keys shouldBe Set(red)
        targetUpdated.edges.isEmpty shouldBe true
    }

}
