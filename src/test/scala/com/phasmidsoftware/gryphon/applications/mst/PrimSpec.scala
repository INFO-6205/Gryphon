package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core.{UndirectedGraph, UndirectedOrderedEdge, UndirectedOrderedEdgeCase}
import org.scalatest.flatspec.AnyFlatSpec

class PrimSpec extends AnyFlatSpec {

    behavior of "Prim"

    it should "mst" in {
        val graph: UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]] = UndirectedGraph[String, Int]("Prim test").addEdge(UndirectedOrderedEdgeCase("A", "B", 1)).addEdge(UndirectedOrderedEdgeCase("A", "D", 3)).addEdge(UndirectedOrderedEdgeCase("A", "C", 2)).asInstanceOf[UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]]]
        val target = new Prim[String, Int]
        val mst = target.mst(graph)
        println(mst)
    }

}
