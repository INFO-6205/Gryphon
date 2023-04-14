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
        val vertexMap: VertexMap[String, DirectedEdgeCase[String, Int]] = OrderedVertexMapCase.empty
        val target = vertexMap.addEdge("A", DirectedEdgeCase("A", "B", 1)).addEdge("B", DirectedEdgeCase("B", "C", 2)).addVertex("C")
        val visitor = Visitor.createPre[String]
        val result = target.dfs(visitor)("A")
        result.journal shouldBe Queue("A", "B", "C")
    }

    it should "bfs" in {
        import Journal._
        val vertexMap: VertexMap[String, DirectedEdgeCase[String, Int]] = OrderedVertexMapCase.empty
        val target = vertexMap.addEdge("A", DirectedEdgeCase("A", "B", 1)).addVertex("B").addEdge("A", DirectedEdgeCase("A", "D", 3)).addVertex("D").addEdge("A", DirectedEdgeCase("A", "C", 2)).addVertex("C")
        val visitor = Visitor.createPre[String]
        val result = target.bfs(visitor)("A")
        result.journal shouldBe Queue("A", "C", "D", "B")
    }

    it should "bfsMutable" in {
        import Journal._
        val vertexMap: VertexMap[String, DirectedEdgeCase[String, Int]] = OrderedVertexMapCase.empty
        val target = vertexMap.addEdge("A", DirectedEdgeCase("A", "B", 1)).addVertex("B").addEdge("A", DirectedEdgeCase("A", "D", 3)).addVertex("D").addEdge("A", DirectedEdgeCase("A", "C", 2)).addVertex("C")
        val visitor = Visitor.createPreQueue[String]
        val result = target.bfsMutable(visitor)("A")
        result match {
            case x: IterableVisitor[String, _] => x.iterator.toSeq shouldBe Seq("A", "C", "D", "B")
        }
    }

    behavior of "OrderedVertexMapCase"

    it should "keys" in {
        val target: VertexMap[String, DirectedEdgeCase[String, Int]] = OrderedVertexMapCase.empty
        target.keys shouldBe Set.empty
    }

    it should "values" in {
        val target: VertexMap[String, DirectedEdgeCase[String, Int]] = OrderedVertexMapCase.empty
        target.values.isEmpty shouldBe true
    }

    it should "addEdge" in {
        val target: VertexMap[String, DirectedEdgeCase[String, Int]] = OrderedVertexMapCase.empty
        val edge: DirectedEdgeCase[String, Int] = DirectedEdgeCase(vertexA, vertexB, 42)
        val targetUpdated = target.addEdge(vertexA, edge)
        targetUpdated.keys shouldBe Set(vertexA)
        targetUpdated.values.toSeq shouldBe Seq(VertexCase("A", AdjacencyList(List(edge))))
        targetUpdated.edges shouldBe Seq(edge)
    }

    it should "addVertex" in {
        val target: VertexMap[String, DirectedEdgeCase[String, Int]] = OrderedVertexMapCase.empty
        val targetUpdated = target.addVertex(vertexA)
        targetUpdated.keys shouldBe Set(vertexA)
        targetUpdated.edges.isEmpty shouldBe true
    }

    behavior of "UnorderedVertexMapCase"

    private val red: Color = Color("red")
    private val blue: Color = Color("blue")

    it should "keys" in {
        val target: VertexMap[Color, DirectedEdgeCase[Color, Int]] = UnorderedVertexMapCase.empty
        target.keys shouldBe Set.empty
    }

    it should "values" in {
        val target: VertexMap[Color, DirectedEdgeCase[Color, Int]] = UnorderedVertexMapCase.empty
        target.values.isEmpty shouldBe true
    }

    it should "addEdge" in {
        val target: VertexMap[Color, DirectedEdgeCase[Color, Int]] = UnorderedVertexMapCase.empty
        val edge: DirectedEdgeCase[Color, Int] = DirectedEdgeCase(red, blue, 42)
        val targetUpdated = target.addEdge(red, edge)
        targetUpdated.keys shouldBe Set(red)
        targetUpdated.edges shouldBe Seq(edge)
    }

    it should "addVertex" in {
        val target: VertexMap[Color, DirectedEdgeCase[Color, Int]] = UnorderedVertexMapCase.empty
        val targetUpdated = target.addVertex(red)
        targetUpdated.keys shouldBe Set(red)
        targetUpdated.edges.isEmpty shouldBe true
    }

}
