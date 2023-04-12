package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.immutable.Queue

class VertexMapSpec extends AnyFlatSpec with should.Matchers {

    private val vertexA = "A"
    private val vertexB = "B"

    behavior of "VertexMap"

    it should "dfs" in {
        import Journal._
        val vertexMap: VertexMap[String, DirectedEdge[String, Int]] = OrderedVertexMap.empty
        val target = vertexMap.addEdge("A", DirectedEdge("A", "B", 1)).addEdge("B", DirectedEdge("B", "C", 2)).addVertex("C")
        val visitor = Visitor.createPre[String]
        val result = target.dfs(visitor)("A")
        result.journal shouldBe Queue("A", "B", "C")
    }

    it should "bfs" in {
        import Journal._
        val vertexMap: VertexMap[String, DirectedEdge[String, Int]] = OrderedVertexMap.empty
        val target = vertexMap.addEdge("A", DirectedEdge("A", "B", 1)).addVertex("B").addEdge("A", DirectedEdge("A", "D", 3)).addVertex("D").addEdge("A", DirectedEdge("A", "C", 2)).addVertex("C")
        val visitor = Visitor.createPre[String]
        val result = target.bfs(visitor)("A")
        result.journal shouldBe Queue("A", "C", "D", "B")
    }

    it should "bfsMutable" in {
        import Journal._
        val vertexMap: VertexMap[String, DirectedEdge[String, Int]] = OrderedVertexMap.empty
        val target = vertexMap.addEdge("A", DirectedEdge("A", "B", 1)).addVertex("B").addEdge("A", DirectedEdge("A", "D", 3)).addVertex("D").addEdge("A", DirectedEdge("A", "C", 2)).addVertex("C")
        val visitor = Visitor.createPreQueue[String]
        val result = target.bfsMutable(visitor)("A")
        result match {
            case x: IterableVisitor[String, _] => x.iterator.toSeq shouldBe Seq("A", "C", "D", "B")
        }
    }

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
        targetUpdated.values.toSeq shouldBe Seq(ConcreteVertex("A", AdjacencyList(List(edge))))
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
