package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EdgeSpec extends AnyFlatSpec with should.Matchers {

    /**
     * Case class for testing where we can have an attribute that does not have an order.
     *
     * @param name the name of the color.
     */
    case class Color(name: String)

    behavior of "DirectedEdge"

    it should "apply" in {
        val target: DirectedEdge[Color, Color] = DirectedEdge(Color("A"), Color("B"), Color("C"))
        target shouldBe new DirectedEdge(Color("A"), Color("B"), Color("C")) // leave "new" intact
    }

    it should "from" in {
        val target: DirectedEdge[Color, String] = DirectedEdge(Color("A"), Color("B"), "isa")
        target.from shouldBe Color("A")
    }

    it should "to" in {
        val target: DirectedEdge[String, String] = DirectedEdge("A", "B", "isa")
        target.to shouldBe "B"
    }

    it should "vertices" in {
        val target: DirectedEdge[String, String] = DirectedEdge("A", "B", "isa")
        target.vertices shouldBe("A", "B")
    }

    it should "attribute" in {
        val target: DirectedEdge[String, String] = DirectedEdge("A", "B", "isa")
        target.attribute shouldBe "isa"
    }

    it should "toString" in {
        val target: DirectedEdge[String, String] = DirectedEdge("A", "B", "isa")
        target.toString shouldBe "A--(isa)-->B"
    }

    behavior of "DirectedOrderedEdge"

    it should "from" in {
        val target: DirectedOrderedEdge[String, String] = DirectedOrderedEdge("A", "B", "isa")
        target.from shouldBe "A"
    }

    it should "to" in {
        val target: DirectedOrderedEdge[String, String] = DirectedOrderedEdge("A", "B", "isa")
        target.to shouldBe "B"
    }

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

    it should "toString" in {
        val target: DirectedOrderedEdge[String, String] = DirectedOrderedEdge("A", "B", "isa")
        target.toString shouldBe "A--(isa)-->B"
    }

    behavior of "UndirectedEdge"

    it should "apply" in {
        val target: UndirectedEdge[String, Color] = UndirectedEdge("A", "B", Color("C"))
        target shouldBe new UndirectedEdge("A", "B", Color("C")) // leave "new" intact
    }

    it should "v1" in {
        val target: UndirectedEdge[String, String] = UndirectedEdge("A", "B", "isa")
        target.v1 shouldBe "A"
    }

    it should "v2" in {
        val target: UndirectedEdge[String, String] = UndirectedEdge("A", "B", "isa")
        target.v2 shouldBe "B"
    }

    it should "vertices" in {
        UndirectedEdge("A", "B", "isa").vertices shouldBe("A", "B")
        UndirectedEdge("B", "A", "isa").vertices shouldBe("A", "B")
    }

    it should "vertex" in {
        UndirectedEdge("A", "B", "isa").vertex shouldBe "A"
        UndirectedEdge("B", "A", "isa").vertex shouldBe "B"
    }

    it should "other" in {
        val ab = UndirectedEdge("A", "B", "isa")
        ab.other(ab.vertex) shouldBe Some("B")
        val ba = UndirectedEdge("B", "A", "has")
        ba.other(ba.vertex) shouldBe Some("A")
    }

    it should "attribute" in {
        val target: UndirectedEdge[String, String] = UndirectedEdge("A", "B", "isa")
        target.attribute shouldBe "isa"
    }

    it should "toString" in {
        val target: UndirectedEdge[String, String] = UndirectedEdge("A", "B", "isa")
        target.toString shouldBe "A<--(isa)-->B"
    }

    behavior of "UndirectedOrderedEdge"

    it should "v1" in {
        val target: UndirectedOrderedEdge[String, String] = UndirectedOrderedEdge("A", "B", "isa")
        target.v1 shouldBe "A"
    }

    it should "v2" in {
        val target: UndirectedOrderedEdge[String, String] = UndirectedOrderedEdge("A", "B", "isa")
        target.v2 shouldBe "B"
    }

    it should "vertices" in {
        val target: UndirectedOrderedEdge[String, String] = UndirectedOrderedEdge("A", "B", "isa")
        target.vertices shouldBe("A", "B")
    }

    it should "attribute" in {
        val target: UndirectedOrderedEdge[String, String] = UndirectedOrderedEdge("A", "B", "isa")
        target.attribute shouldBe "isa"
    }

    it should "vertex" in {
        UndirectedOrderedEdge("A", "B", "isa").vertex shouldBe "A"
        UndirectedOrderedEdge("B", "A", "isa").vertex shouldBe "B"
    }

    it should "other" in {
        val ab = UndirectedOrderedEdge("A", "B", "isa")
        ab.other(ab.vertex) shouldBe Some("B")
        val ba = UndirectedOrderedEdge("B", "A", "has")
        ba.other(ba.vertex) shouldBe Some("A")
    }

    it should "compare with other edge" in {
        val target: UndirectedOrderedEdge[String, Int] = UndirectedOrderedEdge("A", "B", 1)
        val comparand: UndirectedOrderedEdge[String, Int] = UndirectedOrderedEdge("A", "B", 2)
        target.compare(comparand) shouldBe -1
    }

    it should "toString" in {
        val target: UndirectedOrderedEdge[String, String] = UndirectedOrderedEdge("A", "B", "isa")
        target.toString shouldBe "A<--(isa)-->B"
    }

}
