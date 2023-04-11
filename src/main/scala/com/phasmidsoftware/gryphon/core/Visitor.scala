package com.phasmidsoftware.gryphon.core

import scala.collection.immutable.Queue

/**
 * Trait to define the behavior of a visitor--used during depth-first-search, etc.
 * The Visitor supports two journal entries: pre- and post- recursion.
 * Most journals are also iterable so that they can be retrieved after DFS is complete.
 * However, it is perfectly possible to have a journal which simply writes to a file (or something similar).
 *
 * @tparam V the (key) attribute type of a vertex.
 * @tparam J the type of the journal for this visitor.
 */
trait Visitor[V, J] {

    /**
     * Method to visit BEFORE processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V}.
     */
    def visitPre(v: V): Visitor[V, J]

    /**
     * Method to visit AFTER processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V].
     */
    def visitPost(v: V): Visitor[V, J]

    /**
     * Function to process a vertex in pre-order.
     * NOTE This function is not intended for application usage.
     */
    val preFunc: V => J => Option[J]

    /**
     * Function to process a vertex in post-order.
     * NOTE This function is not intended for application usage.
     */
    val postFunc: V => J => Option[J]

    /**
     * The journal of all of the pre- and post- invocations.
     */
    val journal: J
}

object Visitor {
    /**
     * Method to create a composed pre- and post-visitor.
     *
     * @tparam V the vertex type.
     * @return a Visitor[V, List of V].
     */
    def preAndPost[V](implicit ev: Journal[List[V], V]): Visitor[V, List[V]] = PreVisitor[V, List[V]]() join PostVisitor()
}

case class PreVisitor[V, J](journal: J)(implicit val ev: Journal[J, V]) extends BaseVisitor[V, J](journal) {

    val preFunc: V => J => Option[J] = v => a => Some(ev.append(a, v))
    val postFunc: V => J => Option[J] = _ => _ => None

    def unit(journal: J): Visitor[V, J] = PreVisitor(journal)
}


object PreVisitor {
    def apply[V, J]()(implicit ev: Journal[J, V]): PreVisitor[V, J] = new PreVisitor(ev.empty)

    def create[V](implicit ev: Journal[Queue[V], V]): PreVisitor[V, Queue[V]] = PreVisitor[V, Queue[V]]()

    def reverse[V](implicit ev: Journal[List[V], V]): PreVisitor[V, List[V]] = new PreVisitor(ev.empty)

}

case class PostVisitor[V, J](journal: J)(implicit val ev: Journal[J, V]) extends BaseVisitor[V, J](journal) {

    val postFunc: V => J => Option[J] = v => a => Some(ev.append(a, v))
    val preFunc: V => J => Option[J] = _ => _ => None

    def unit(journal: J): Visitor[V, J] = PostVisitor(journal)
}

object PostVisitor {
    def apply[V, J]()(implicit ev: Journal[J, V]): PostVisitor[V, J] = new PostVisitor(ev.empty)

    def create[V](implicit ev: Journal[Queue[V], V]): PostVisitor[V, Queue[V]] = PostVisitor[V, Queue[V]]()

    def reverse[V](implicit ev: Journal[List[V], V]): PostVisitor[V, List[V]] = new PostVisitor(ev.empty)
}

/**
 * Concrete Visitor which is defined by its provided pre and post functions.
 *
 * @param preFunc  the function to be invoked in pre-order.
 * @param postFunc the function to be invoked in post-order.
 * @param journal  the journal structure.
 * @tparam V the (key) vertex-type.
 * @tparam J the Journal type.
 *           Requires implicit evidence of Journal[J, V].
 */
class GenericVisitor[V, J](val preFunc: V => J => Option[J], val postFunc: V => J => Option[J])(val journal: J)(implicit val ev: Journal[J, V]) extends BaseVisitor[V, J](journal) {
    def unit(journal: J): Visitor[V, J] = new GenericVisitor(preFunc, postFunc)(journal)
}

abstract class BaseVisitor[V, J](journal: J)(implicit val ava: Journal[J, V]) extends Visitor[V, J] {
    self =>

    /**
     * Method to visit before processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V}.
     */
    def visitPre(v: V): Visitor[V, J] = unit(preFunc(v)(journal) getOrElse journal)

    /**
     * Method to visit after processing the (child) V values.
     *
     * @param v (V) the value of this node (vertex).
     * @return an updated Visitor[V].
     */
    def visitPost(v: V): Visitor[V, J] = unit(postFunc(v)(journal) getOrElse journal)

    def unit(journal: J): Visitor[V, J]

    /**
     * Method to compose two Visitors into one.
     *
     * NOTE that the types (V, J) of the other visitor MUST be consistent with the types of this visitor.
     * See VisitorSpec to see how you might work around this limitation.
     *
     * @param visitor a Visitor[V, J].
     * @return a new GenericVisitor[V, J].
     */
    def join(visitor: Visitor[V, J]): Visitor[V, J] =
        new GenericVisitor[V, J](v => a => joinFunc(a, self.preFunc(v), visitor.preFunc(v)), v => a => joinFunc(a, self.postFunc(v), visitor.postFunc(v)))(self.journal)

    private def joinFunc(a: J, f1: J => Option[J], f2: J => Option[J]) = f1(a) match {
        case x@Some(b) => f2(b) orElse x
        case None => f2(a)
    }
}

abstract class BaseIterableVisitor[V, J <: Iterable[V]](appendable: J)(implicit val avai: IterableJournal[J, V]) extends BaseVisitor[V, J](appendable) {
    self =>

    def iterator: Iterator[V] = avai.iterator(appendable)

    def unit(journal: J): Visitor[V, J]

    private def joinFunc(a: J, f1: J => Option[J], f2: J => Option[J]) = f1(a) match {
        case x@Some(b) => f2(b) orElse x
        case None => f2(a)
    }
}

trait HasIterator[J <: Iterable[V], V] {
    def iterator(a: J): Iterator[V] = a.iterator
}

/**
 * Trait to define the behavior of a Journal of V elements.
 * It is used in the Visitor trait as something which can receive the V elements.
 * J Journal does not support an iterator, since many journals might simply be a log file of some sort.
 *
 * @tparam J the journal type.
 * @tparam V the underlying type of the journal.
 */
trait Journal[J, V] {

    /**
     * An empty journal.
     */
    val empty: J

    /**
     * Method to append a V value to a journal.
     *
     * @param j the journal to be appended to.
     * @param v an instance of V to be appended to the journal j.
     * @return a new journal.
     */
    def append(j: J, v: V): J
}

/**
 * Trait which combines the behaviors of Journal and HasIterator.
 *
 * @tparam J the (iterable) journal type for this IterableJournal (J must be a sub-class of Iterable[V]).
 * @tparam V the underlying type of the journal.
 */
trait IterableJournal[J <: Iterable[V], V] extends Journal[J, V] with HasIterator[J, V]

trait JournalQueue[V] extends Journal[Queue[V], V] with HasIterator[Queue[V], V] {
    val empty: Queue[V] = Queue.empty

    def append(q: Queue[V], v: V): Queue[V] = q.enqueue(v)
}

trait JournalStack[V] extends Journal[List[V], V] with HasIterator[List[V], V] {
    val empty: List[V] = List.empty

    def append(q: List[V], v: V): List[V] = v :: q
}

object Journal {
    implicit object JournalQueueInt$ extends JournalQueue[Int]

    implicit object JournalStackInt$ extends JournalStack[Int]
}
