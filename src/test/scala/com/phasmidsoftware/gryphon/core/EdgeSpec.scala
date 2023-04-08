package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EdgeSpec extends AnyFlatSpec with should.Matchers {

    behavior of "DirectedEdge"

    it should "vertices" in {
        val target: DirectedEdge[String, String] = DirectedEdge("A", "B", "isa")
        target.vertices shouldBe("A", "B")
    }

    it should "attribute" in {
        val target: DirectedEdge[String, String] = DirectedEdge("A", "B", "isa")
        target.attribute shouldBe "isa"
    }

    behavior of "DirectedOrderedEdge"

    it should "vertices" in {
        val target: DirectedOrderedEdge[String, String] = DirectedOrderedEdge("A", "B", "isa")
        target.vertices shouldBe("A", "B")
    }

    it should "attribute" in {
        val target: DirectedOrderedEdge[String, String] = DirectedOrderedEdge("A", "B", "isa")
        target.attribute shouldBe "isa"
    }

    it should "compare with other edge" in {
        val target: DirectedOrderedEdge[String, Int] = DirectedOrderedEdge("A", "B", 1)
        val comparand: DirectedOrderedEdge[String, Int] = DirectedOrderedEdge("A", "B", 2)
        target.compare(comparand) shouldBe -1
    }

    behavior of "UndirectedEdge"

    it should "vertices" in {
        val target: UndirectedEdge[String, String] = UndirectedEdge("A", "B", "isa")
        target.vertices shouldBe("A", "B")
    }

    it should "attribute" in {
        val target: UndirectedEdge[String, String] = UndirectedEdge("A", "B", "isa")
        target.attribute shouldBe "isa"
    }

    behavior of "UndirectedOrderedEdge"

    it should "vertices" in {
        val target: UndirectedOrderedEdge[String, String] = UndirectedOrderedEdge("A", "B", "isa")
        target.vertices shouldBe("A", "B")
    }

    it should "attribute" in {
        val target: UndirectedOrderedEdge[String, String] = UndirectedOrderedEdge("A", "B", "isa")
        target.attribute shouldBe "isa"
    }

    it should "compare with other edge" in {
        val target: UndirectedOrderedEdge[String, Int] = UndirectedOrderedEdge("A", "B", 1)
        val comparand: UndirectedOrderedEdge[String, Int] = UndirectedOrderedEdge("A", "B", 2)
        target.compare(comparand) shouldBe -1
    }

}
