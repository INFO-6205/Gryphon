package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.immutable.Queue

class VisitorSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Visitor"

    it should "PostVisitor" in {
        val target: PostVisitor[Int, Queue[Int]] = PostVisitor()
        val queue = Queue.empty[Int]
        val f: Int => Queue[Int] => Option[Queue[Int]] = target.preFunc
        val a1: Option[Queue[Int]] = f(1)(queue)
        a1 shouldBe None
        val g: Int => Queue[Int] => Option[Queue[Int]] = target.postFunc
        val a2: Option[Queue[Int]] = g(1)(queue)
        a2 shouldBe Some(Queue(1))
    }

    it should "PreVisitor" in {
        val target: PreVisitor[Int, Queue[Int]] = PreVisitor()
        val queue = Queue.empty[Int]
        val f: Int => Queue[Int] => Option[Queue[Int]] = target.preFunc
        val a1: Option[Queue[Int]] = f(1)(queue)
        a1 shouldBe Some(Queue(1))
        val g: Int => Queue[Int] => Option[Queue[Int]] = target.postFunc
        val a2: Option[Queue[Int]] = g(1)(queue)
        a2 shouldBe None
    }

    it should "preFunc" in {
        val target: PreVisitor[Int, Queue[Int]] = PreVisitor()
        val f: Int => Queue[Int] => Option[Queue[Int]] = target.preFunc
        val queue = Queue.empty[Int]
        val a1: Option[Queue[Int]] = f(1)(queue)
        a1 shouldBe Some(Queue(1))
    }

//    it should "join" in {
//        val preVisitor: PreVisitor[Int] = PreVisitor()
//        val postVisitor: PostVisitor[Int] = PostVisitor()
//        val target: Visitor[Int, Queue[Int]] = preVisitor join postVisitor
//        val queue = Queue.empty[Int]
//        val z: Visitor[Int, Queue[Int]] = target.visitPre(queue)(1)
//        z shouldBe Queue.empty
//    }

    it should "postFunc" in {
        val target: PostVisitor[Int, Queue[Int]] = PostVisitor()
        val f: Int => Queue[Int] => Option[Queue[Int]] = target.postFunc
        val queue = Queue.empty[Int]
        val a1: Option[Queue[Int]] = f(1)(queue)
        a1 shouldBe Some(Queue(1))
    }

}
