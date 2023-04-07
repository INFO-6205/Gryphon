package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GraphSpec extends AnyFlatSpec with should.Matchers {

    behavior of "UndirectedGraph"

    it should "create an empty graph" in {
        val target: UndirectedGraph[String, String] = UndirectedGraph[String, String]
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }

    behavior of "DirectedGraph"

    it should "create an empty graph" in {
        val target: DirectedGraph[String, String] = DirectedGraph[String, String]
        target.edges shouldBe Nil
        target.vertices shouldBe Set.empty
    }
}
