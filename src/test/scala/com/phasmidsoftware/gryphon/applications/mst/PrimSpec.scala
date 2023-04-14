package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core.{UndirectedEdgeCase, UndirectedGraph, UndirectedOrderedEdge}
import org.scalatest.flatspec.AnyFlatSpec

class PrimSpec extends AnyFlatSpec {

    behavior of "Prim"

    ignore should "mst" in {
        val graph: UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]] = UndirectedGraph[String, Int]("Prim test").addEdge(UndirectedEdgeCase("A", "B", 1)).addEdge(UndirectedEdgeCase("A", "D", 3)).addEdge(UndirectedEdgeCase("A", "C", 2)).asInstanceOf[UndirectedGraph[String, Int, UndirectedOrderedEdge[String, Int]]]
        val target = new Prim[String, Int]
        val mst = target.mst(graph)
        println(mst)
    }

}
