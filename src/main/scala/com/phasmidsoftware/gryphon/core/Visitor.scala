package com.phasmidsoftware.gryphon.core

import scala.collection.immutable.Queue

/**
 * Trait to define the behavior of a visitor--used during depth-first-search, etc.
 *
 * @tparam V the (key) attribute type of a vertex.
 */
trait Visitor[V, A] {

    /**
     * Method to visit BEFORE processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V}.
     */
    def visitPre(v: V): Visitor[V, A]

    /**
     * Method to visit AFTER processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V].
     */
    def visitPost(v: V): Visitor[V, A]

    /**
     * Function to process a vertex in pre-order.
     * NOTE This function is not intended for application usage.
     */
    val preFunc: V => A => Option[A]

    /**
     * Function to process a vertex in post-order.
     * NOTE This function is not intended for application usage.
     */
    val postFunc: V => A => Option[A]

    /**
     * The record of all of the pre- and post- invocations.
     */
    val appendable: A
}

object Visitor {
    /**
     * Method to create a composed pre- and post-visitor.
     *
     * @tparam V the vertex type.
     * @return a Visitor[V, List of V].
     */
    def preAndPost[V](implicit ev: Appendable[List[V], V]): Visitor[V, List[V]] = PreVisitor[V, List[V]]() join PostVisitor()
}
case class PreVisitor[V, A](appendable: A)(implicit val ev: Appendable[A, V]) extends BaseVisitor[V, A](appendable) {

    val preFunc: V => A => Option[A] = v => a => Some(ev.append(a, v))
    val postFunc: V => A => Option[A] = _ => _ => None

    def unit(a: A): Visitor[V, A] = PreVisitor(a)
}


object PreVisitor {
    def apply[V, A]()(implicit ev: Appendable[A, V]): PreVisitor[V, A] = new PreVisitor(ev.empty)

    def create[V](implicit ev: Appendable[Queue[V], V]): PreVisitor[V, Queue[V]] = PreVisitor[V, Queue[V]]()

    def reverse[V](implicit ev: Appendable[List[V], V]): PreVisitor[V, List[V]] = new PreVisitor(ev.empty)

}

case class PostVisitor[V, A](appendable: A)(implicit val ev: Appendable[A, V]) extends BaseVisitor[V, A](appendable) {

    val postFunc: V => A => Option[A] = v => a => Some(ev.append(a, v))
    val preFunc: V => A => Option[A] = _ => _ => None

    def unit(a: A): Visitor[V, A] = PostVisitor(a)
}

object PostVisitor {
    def apply[V, A]()(implicit ev: Appendable[A, V]): PostVisitor[V, A] = new PostVisitor(ev.empty)

    def create[V](implicit ev: Appendable[Queue[V], V]): PostVisitor[V, Queue[V]] = PostVisitor[V, Queue[V]]()

    def reverse[V](implicit ev: Appendable[List[V], V]): PostVisitor[V, List[V]] = new PostVisitor(ev.empty)
}

/**
 * Concrete Visitor which is defined by its provided pre and post functions.
 *
 * @param preFunc    the function to be invoked in pre-order.
 * @param postFunc   the function to be invoked in post-order.
 * @param appendable the appendable structure.
 * @tparam V the (key) vertex-type.
 * @tparam A the Appendable type.
 *           Requires implicit evidence of Appendable[A, V].
 */
class GenericVisitor[V, A](val preFunc: V => A => Option[A], val postFunc: V => A => Option[A])(val appendable: A)(implicit val ev: Appendable[A, V]) extends BaseVisitor[V, A](appendable) {
    def unit(a: A): Visitor[V, A] = new GenericVisitor(preFunc, postFunc)(a)
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

    /**
     * Method to compose two Visitors into one.
     *
     * NOTE that the types (V, A) of the other visitor MUST be consistent with the types of this visitor.
     * See VisitorSpec to see how you might work around this limitation.
     *
     * @param visitor a Visitor[V, A].
     * @return a new GenericVisitor[V, A].
     */
    def join(visitor: Visitor[V, A]): Visitor[V, A] =
        new GenericVisitor[V, A](v => a => joinFunc(a, self.preFunc(v), visitor.preFunc(v)), v => a => joinFunc(a, self.postFunc(v), visitor.postFunc(v)))(self.appendable)

    private def joinFunc(a: A, f1: A => Option[A], f2: A => Option[A]) = f1(a) match {
        case x@Some(b) => f2(b) orElse x
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

    def append(q: List[V], v: V): List[V] = v :: q
}

object Appendable {
    implicit object AppendableQueueInt extends AppendableQueue[Int]

    implicit object AppendableStackInt extends AppendableStack[Int]

}
