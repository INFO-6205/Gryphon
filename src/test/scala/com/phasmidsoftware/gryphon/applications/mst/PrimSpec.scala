package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.applications.mst.GraphBuilder.{createFromUndirectedEdgeList, resource}
import com.phasmidsoftware.gryphon.core.{UndirectedGraph, UndirectedOrderedEdge, UndirectedOrderedEdgeCase}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

class PrimSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Prim"

    it should "mst of trivial graph" in {
        val graph: UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]] = UndirectedGraph[String, Int]("Prim test").addEdge(UndirectedOrderedEdgeCase("A", "B", 1)).addEdge(UndirectedOrderedEdgeCase("A", "D", 3)).addEdge(UndirectedOrderedEdgeCase("A", "C", 2)).asInstanceOf[UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]]]
        val target = new Prim[String, Int]
        val mst = target.mst(graph)
        println(mst)
    }
    it should "mst of Prim demo from Sedgewick & Wayne" in {
        val uy = resource("/prim.graph")
        val gy = createFromUndirectedEdgeList[Int, Double](uy)(w => Try(w.toInt), w => Try(w.toDouble))
        gy match {
            case Success(graph) =>
                println("graph edges/vertices")
                graph.edges foreach println
                graph.vertices foreach println
                val target = new Prim[Int, Double]
                val mst = target.mst(graph.asInstanceOf[UndirectedGraph[Int, Double, UndirectedOrderedEdge[Int, Double]]])
                println("MST edges/vertices")
                mst.edges foreach println
                mst.vertices foreach println
                mst.edges.size shouldBe 7
                // FIXME
//                mst.vertices.size shouldBe 8
            case Failure(x) => throw x
        }

    }

}
