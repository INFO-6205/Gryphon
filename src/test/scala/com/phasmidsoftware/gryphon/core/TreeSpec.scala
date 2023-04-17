package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.immutable.HashMap

class TreeSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Tree"

    private val red: String = "red"
    private val blue: String = "blue"
    private val green: String = "green"

    it should "unit" in {
        val vertexMap = UnorderedVertexMap.empty[String, UndirectedEdge[String, Int]].asInstanceOf[BaseVertexMap[String, UndirectedEdge[String, Int]]]
        val tree1 = TreeCase[String, Int]("test1", vertexMap)
        val edge42: UndirectedEdge[String, Int] = UndirectedEdgeCase(red, blue, 42)
        val edge17: UndirectedEdge[String, Int] = UndirectedEdgeCase(red, green, 17)
        val vm2 = vertexMap.unit(buildUpVertexMap(vertexMap, edge42, edge17)).asInstanceOf[BaseVertexMap[String, UndirectedEdge[String, Int]]]
        val tree2 = tree1.unit(vm2)
        tree2.vertexMap shouldBe vm2
        tree2.edges shouldBe Seq(edge42, edge17)
    }

    it should "isCyclic" in {
        val vertexMap: BaseVertexMap[String, UndirectedEdge[String, Int]] = UnorderedVertexMap.empty[String, UndirectedEdge[String, Int]].asInstanceOf[BaseVertexMap[String, UndirectedEdge[String, Int]]]
        val edge42: UndirectedEdge[String, Int] = UndirectedEdgeCase(red, blue, 42)
        val edge17: UndirectedEdge[String, Int] = UndirectedEdgeCase(red, green, 17)
        val vm2 = vertexMap.unit(buildUpVertexMap(vertexMap, edge42, edge17)).asInstanceOf[BaseVertexMap[String, UndirectedEdge[String, Int]]]
        val tree = TreeCase[String, Int]("test", vm2)
        tree.vertexMap shouldBe vm2
        tree.edges shouldBe Seq(edge42, edge17)
        tree.isCyclic shouldBe false
    }

    private def buildUpVertexMap(vertexMap: BaseVertexMap[String, UndirectedEdge[String, Int]], edge42: UndirectedEdge[String, Int], edge17: UndirectedEdge[String, Int]) = {
        val vRed = Vertex.empty(red)
        val vBlue = Vertex.empty(blue)
        val vGreen = Vertex.empty(green)
        val m1 = new HashMap[String, Vertex[String, UndirectedEdge[String, Int]]]()
        val m2 = vertexMap.buildMap(m1, red, edge42, vRed)
        val m3 = vertexMap.buildMap(m2, blue, edge42, vBlue)
        val m4 = vertexMap.buildMap(m3, red, edge17, vRed)
        vertexMap.buildMap(m4, green, edge17, vGreen)
    }
}
