package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core.{UndirectedGraph, UndirectedOrderedEdge, UndirectedOrderedEdgeCase}
import com.phasmidsoftware.gryphon.util.CsvParser.parseUndirectedEdgeList
import com.phasmidsoftware.gryphon.util.GraphBuilder.{createFromUndirectedEdgeList, createGraphFromUndirectedOrderedEdges, resource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

class LazyPrimSpec extends AnyFlatSpec with should.Matchers {

    behavior of "LazyPrim"

    it should "mst of trivial graph" in {
        val edge1 = UndirectedOrderedEdgeCase("A", "B", 1)
        val edge3 = UndirectedOrderedEdgeCase("A", "D", 3)
        val edge2 = UndirectedOrderedEdgeCase("A", "C", 2)
        val graph: UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]] = UndirectedGraph[String, Int]("Prim test").addEdge(edge1).addEdge(edge3).addEdge(edge2).asInstanceOf[UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]]]
        val target = new LazyPrim[String, Int]
        val mst = target.mst(graph)
        mst.edges shouldBe List(edge3, edge2, edge1)
    }

    it should "mst of Prim demo from Sedgewick & Wayne" in {
        val uy = resource("/prim.graph")
        val esy = createFromUndirectedEdgeList[Int, Double](uy)(w => Try(w.toInt), w => Try(w.toDouble))
        createGraphFromUndirectedOrderedEdges(esy) match {
            case Success(graph) =>
                val target = new LazyPrim[Int, Double]
                val mst = target.mst(graph.asInstanceOf[UndirectedGraph[Int, Double, UndirectedOrderedEdge[Int, Double]]])
                mst.edges.size shouldBe 7
                mst.vertices.size shouldBe 8
                mst.edges map (_.attribute) shouldBe List(0.26, 0.16, 0.4, 0.17, 0.35, 0.28, 0.19)
                mst.vertices shouldBe Set(0, 1, 2, 3, 4, 5, 6, 7)
            case Failure(x) => throw x
        }
    }

    // FIXME
    ignore should "mst the traveling salesman problem for INFO6205 Spring 2023" in {
        val spring2023Project = "/info6205.spring2023.teamproject.csv"
        val uy = resource(spring2023Project)
        parseUndirectedEdgeList[Int, Double](spring2023Project)
        val esy = createFromUndirectedEdgeList[Int, Double](uy)(w => Try(w.toInt), w => Try(w.toDouble))
        createGraphFromUndirectedOrderedEdges(esy) match {
            case Success(graph) =>
                val target = new LazyPrim[Int, Double]
                val mst = target.mst(graph.asInstanceOf[UndirectedGraph[Int, Double, UndirectedOrderedEdge[Int, Double]]])
                mst.edges.size shouldBe 7
                mst.vertices.size shouldBe 8
                mst.edges map (_.attribute) shouldBe List(0.26, 0.16, 0.4, 0.17, 0.35, 0.28, 0.19)
                mst.vertices shouldBe Set(0, 1, 2, 3, 4, 5, 6, 7)
            case Failure(x) => throw x
        }

    }
}
