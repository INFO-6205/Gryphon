package com.phasmidsoftware.gryphon.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.immutable.Queue

class VisitorSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Visitor"

    it should "PostVisitor" in {
        val target: PostVisitor[Int, Queue[Int]] = PostVisitor()
        val t1 = target.visitPost(1)
        t1 shouldBe PostVisitor(Queue(1))
        val queue = Queue.empty[Int]
        val a1: Option[Queue[Int]] = target.preFunc(1)(queue)
        a1 shouldBe None
        val a2: Option[Queue[Int]] = target.postFunc(1)(queue)
        a2 shouldBe Some(Queue(1))
        val a3: Option[Queue[Int]] = target.postFunc(2)(a2.get)
        a3 shouldBe Some(Queue(1, 2))
    }

    it should "PreVisitor" in {
        val target: PreVisitor[Int, Queue[Int]] = PreVisitor()
        val queue = Queue.empty[Int]
        val a1: Option[Queue[Int]] = target.preFunc(1)(queue)
        a1 shouldBe Some(Queue(1))
        val a2: Option[Queue[Int]] = target.postFunc(1)(queue)
        a2 shouldBe None
        val a3: Option[Queue[Int]] = target.preFunc(2)(a1.get)
        a3 shouldBe Some(Queue(1, 2))
    }

    it should "PostVisitor.reverse" in {
        val target: PostVisitor[Int, List[Int]] = PostVisitor.reverse
        target.visitPre(1) shouldBe target
        val t1 = target.visitPost(1)
        t1 shouldBe PostVisitor(List(1))
        val t2 = t1.visitPost(2)
        t2 shouldBe PostVisitor(List(2, 1))
    }

    it should "preFunc" in {
        val target: PreVisitor[Int, Queue[Int]] = PreVisitor()
        val queue = Queue.empty[Int]
        val a1: Option[Queue[Int]] = target.preFunc(1)(queue)
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
        val queue = Queue.empty[Int]
        val a1: Option[Queue[Int]] = target.postFunc(1)(queue)
        a1 shouldBe Some(Queue(1))
    }

}
