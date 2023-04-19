package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.util.GraphBuilder.resource
import com.phasmidsoftware.gryphon.util.{EdgeDataParser, GraphBuilder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

class GraphBuilderSpec extends AnyFlatSpec with should.Matchers {

    behavior of "GraphBuilder"

    it should "createFromUndirectedEdgeList" in {
        val primGraph = "/prim.graph"
        val uy = resource(primGraph)
        val graphBuilder = new GraphBuilder[Int, Double, Unit]()
        val esy = graphBuilder.createFromUndirectedEdgeList(uy)(w => Try(w.toInt), w => Try(w.toDouble))
        graphBuilder.createGraphFromUndirectedOrderedEdges(esy) match {
            case Success(g) =>
                val edges = g.edges
                edges.size shouldBe 16
            case Failure(x) => throw x
        }
    }

    it should "parseUndirectedEdgeList" in {
        val parser = new EdgeDataParser[Int, Double]()
        val primGraph = "/prim.graph"
        val graphBuilder = new GraphBuilder[Int, Double, Unit]()
        graphBuilder.createGraphFromUndirectedOrderedEdges(parser.parseEdgesFromCsv(primGraph)) match {
            case Success(g) =>
                val edges = g.edges
                edges.size shouldBe 16
            case Failure(x) => throw x
        }
    }
}
