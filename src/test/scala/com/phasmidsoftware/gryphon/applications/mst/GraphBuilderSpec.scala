package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.util.GraphBuilder.{createFromUndirectedEdgeList, resource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

class GraphBuilderSpec extends AnyFlatSpec with should.Matchers {

    behavior of "GraphBuilder"

    it should "createFromUndirectedEdgeList" in {
         val uy = resource("/prim.graph")
         val gy = createFromUndirectedEdgeList[Int, Double](uy)(w => Try(w.toInt), w => Try(w.toDouble))
        gy match {
            case Success(g) =>
                val edges = g.edges
                edges.size shouldBe 16
            case Failure(x) => throw x
        }

    }

}
