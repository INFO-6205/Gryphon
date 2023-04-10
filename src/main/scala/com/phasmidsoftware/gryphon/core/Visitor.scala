package com.phasmidsoftware.gryphon.core

import scala.collection.immutable.Queue

/**
 * Trait to define the behavior of a visitor--used during depth-first-search, etc.
 *
 * @tparam V the (key) attribute type of a vertex.
 */
trait Visitor[V, A] {

    self =>

    val preFunc: V => A => Option[A]

    val postFunc: V => A => Option[A]

    val appendable: A

    /**
     * Method to visit before processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V}.
     */
    def visitPre(v: V): Visitor[V, A]

    /**
     * Method to visit after processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V].
     */
    def visitPost(v: V): Visitor[V, A]
}

case class PreVisitor[V, A](appendable: A)(implicit val ev: Appendable[A, V]) extends BaseVisitor[V, A](appendable) {

    val preFunc: V => A => Option[A] = v => a => Some(ev.append(a, v))
    val postFunc: V => A => Option[A] = _ => _ => None

    def unit(a: A): Visitor[V, A] = PreVisitor(a)
}


object PreVisitor {
    def apply[V, A]()(implicit ev: Appendable[A, V]): PreVisitor[V, A] = new PreVisitor(ev.empty)
}


case class PostVisitor[V, A](appendable: A)(implicit val ev: Appendable[A, V]) extends BaseVisitor[V, A](appendable) {

    val postFunc: V => A => Option[A] = v => a => Some(ev.append(a, v))
    val preFunc: V => A => Option[A] = _ => _ => None

    def unit(a: A): Visitor[V, A] = PostVisitor(a)

}

object PostVisitor {
    def apply[V, A]()(implicit ev: Appendable[A, V]): PostVisitor[V, A] = new PostVisitor(ev.empty)
}

abstract class BaseVisitor[V, A](appendable: A)(implicit val ava: Appendable[A, V]) extends Visitor[V, A] {
    self =>

    /**
     * Method to visit before processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V}.
     */
    def visitPre(v: V): Visitor[V, A] = unit(preFunc(v)(appendable) getOrElse appendable)

    /**
     * Method to visit after processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V].
     */
    def visitPost(v: V): Visitor[V, A] = unit(postFunc(v)(appendable) getOrElse appendable)

    def unit(appendable: A): Visitor[V, A]

//    def join(visitor: Visitor[V, A]): Visitor[V, A] = new BaseVisitor[V, A](self.appendable) {
//        val preFunc: V => A => Option[A] = v => a => joinFunc(a, self.preFunc(v), visitor.preFunc(v))
//        val postFunc: V => A => Option[A] = v => a => joinFunc(a, self.postFunc(v), visitor.postFunc(v))
//
//        def unit(a: A): Visitor[V, A] = self.unit(a)
//    }

    private def joinFunc(a: A, f1: A => Option[A], f2: A => Option[A]) = f1(a) match {
        case Some(b) => f2(b)
        case None => f2(a)
    }
}

trait Appendable[A, V] {

    val empty: A

//    def combine(other: A): Appendable[A, V]

    def append(a: A, v: V): A
}

trait AppendableQueue[V] extends Appendable[Queue[V], V] {
    val empty: Queue[V] = Queue.empty

//    def combine(other: Queue[V]): Appendable[Queue[V], V] = ???

    def append(q: Queue[V], v: V): Queue[V] = q.enqueue(v)
}

trait AppendableStack[V] extends Appendable[List[V], V] {
    val empty: List[V] = List.empty

//    def combine(other: Queue[V]): Appendable[Queue[V], V] = ???

    def append(q: List[V], v: V): List[V] = q :+ v
}

object Appendable {
    implicit object AppendableQueueInt extends AppendableQueue[Int]

    implicit object AppendableStackInt extends AppendableStack[Int]

}
