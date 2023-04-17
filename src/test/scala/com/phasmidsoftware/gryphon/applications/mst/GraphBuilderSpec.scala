package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.util.CsvParser.parseUndirectedEdgeList
import com.phasmidsoftware.gryphon.util.GraphBuilder.{createFromUndirectedEdgeList, createGraphFromUndirectedOrderedEdges, resource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

class GraphBuilderSpec extends AnyFlatSpec with should.Matchers {

    behavior of "GraphBuilder"

    it should "createFromUndirectedEdgeList" in {
        val primGraph = "/prim.graph"
        val uy = resource(primGraph)
        val esy = createFromUndirectedEdgeList[Int, Double](uy)(w => Try(w.toInt), w => Try(w.toDouble))
        createGraphFromUndirectedOrderedEdges(esy) match {
            case Success(g) =>
                val edges = g.edges
                edges.size shouldBe 16
            case Failure(x) => throw x
        }
    }

    it should "parseUndirectedEdgeList" in {
        val primGraph = "/prim.graph"
        createGraphFromUndirectedOrderedEdges(parseUndirectedEdgeList[Int, Double](primGraph)) match {
            case Success(g) =>
                val edges = g.edges
                edges.size shouldBe 16
            case Failure(x) => throw x
        }
    }
}
