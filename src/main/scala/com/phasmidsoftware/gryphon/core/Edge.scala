package com.phasmidsoftware.gryphon.core

/**
 * Trait to model the behavior of an Edge.
 *
 * @tparam V the Vertex key type, i.e. the type of its attribute.
 * @tparam E the Edge type, i.e. the type of its attribute.
 */
trait Edge[V, E] extends EdgeLike[V] {
    /**
     * The attribute of this Edge.
     *
     * @return
     */
    val attribute: E
}

/**
 * Abstract base class to represent an undirected edge.
 *
 * @param _v1        (V) one vertex attribute (key).
 * @param _v2        (V) the other vertex attribute (key).
 * @param _attribute the edge attribute.
 * @tparam V the Vertex key type, i.e. the type of its attribute.
 *           Needs evidence of Ordering[V].
 * @tparam E the Edge type, i.e. the type of its attribute.
 */
abstract class BaseUndirectedEdge[V: Ordering, E](_v1: V, _v2: V, val _attribute: E) extends Edge[V, E] with Undirected[V] {
    /**
     * Value of _v1.
     *
     * NOTE could equally take the value of <code>_v2</code> but note that method vertices relies on this being deterministic.
     */
    val vertex: V = _v1

    /**
     * Method to return the other end of this edge from the given vertex <code>v</code>.
     * If the value of <code>v</code> is neither <code>_v1</code> nor <code>_v2</code>, then None will be returned.
     *
     * @param v the given vertex key (attribute).
     * @return an optional vertex key.
     */
    def other(v: V): Option[V] = Option.when(v == _v1)(_v2) orElse Option.when(v == _v2)(_v1)

    /**
     * Method to get the two vertices of this edge in some deterministic order, based on the implicit value of Ordering[V].
     */
    val vertices: (V, V) = {
        val v = if (implicitly[Ordering[V]].compare(_v1, _v2) <= 0) _v1 else _v2
        vertex -> other(v).get // NOTE this is guaranteed to have a value.
    }
}

/**
 * Abstract base class for a directed edge.
 *
 * @param _from      (V) the start vertex attribute (key).
 * @param _to        (V) the end vertex attribute (key).
 * @param _attribute (E) the edge attribute.
 * @tparam V the Vertex key type, i.e. the type of its attribute.
 * @tparam E the Edge type, i.e. the type of its attribute.
 */
abstract class BaseDirectedEdge[V, E](val _from: V, val _to: V, val _attribute: E) extends Edge[V, E] with Directed[V] {
    /**
     * The two vertices in the natural order: _from, _to.
     */
    val vertices: (V, V) = _from -> _to
}

/**
 * Abstract base class for an undirected, ordered edge.
 * For example, an edge with a weighting.
 *
 * @param _v1        (V) one vertex attribute (key).
 * @param _v2        (V) the other vertex attribute (key).
 * @param _attribute the edge attribute.
 * @tparam V the Vertex key type, i.e. the type of its attribute.
 *           Needs evidence of Ordering[V].
 * @tparam E the Edge type, i.e. the type of its attribute.
 *           Needs evidence of Ordering[E].
 */
abstract class BaseUndirectedOrderedEdge[V: Ordering, E: Ordering](_v1: V, _v2: V, override val _attribute: E) extends BaseUndirectedEdge[V, E](_v1, _v2, _attribute) with Ordered[Edge[V, E]] {

    /**
     * Method to compare this edge with that edge.
     *
     * @param that another edge.
     * @return -1, 0, or 1 depending on the ordering of this and that edges.
     */
    def compare(that: Edge[V, E]): Int = BaseOrderedEdge.compare(this, that)
}

/**
 * Abstract base class for an directed, ordered edge.
 * For example, an edge with a weighting.
 *
 * @param _from      (V) start vertex attribute (key).
 * @param _to        (V) the end vertex attribute (key).
 * @param _attribute (E) the edge attribute
 * @tparam V the Vertex key type, i.e. the type of its attribute.
 * @tparam E the Edge type, i.e. the type of its attribute.
 *           Needs evidence of Ordering[E].
 */
abstract class BaseDirectedOrderedEdge[V, E: Ordering](override val _from: V, override val _to: V, override val _attribute: E) extends BaseDirectedEdge[V, E](_from, _to, _attribute) with Ordered[Edge[V, E]] {

    /**
     * Method to compare this edge with that edge.
     *
     * @param that another edge.
     * @return -1, 0, or 1 depending on the ordering of this and that edges.
     */
    def compare(that: Edge[V, E]): Int = BaseOrderedEdge.compare(this, that)
}

case class DirectedEdge[V, E](from: V, to: V, attribute: E) extends BaseDirectedEdge[V, E](from, to, attribute)

case class DirectedOrderedEdge[V, E: Ordering](from: V, to: V, attribute: E) extends BaseDirectedOrderedEdge[V, E](from, to, attribute)

case class UndirectedEdge[V: Ordering, E](v1: V, v2: V, attribute: E) extends BaseUndirectedEdge[V, E](v1, v2, attribute)

case class UndirectedOrderedEdge[V: Ordering, E: Ordering](v1: V, v2: V, attribute: E) extends BaseUndirectedOrderedEdge[V, E](v1, v2, attribute)

trait EdgeLike[V] {

    /**
     * The two vertices of this Edge in a (possibly) arbitrary but deterministic order.
     */
    val vertices: (V, V)
}

trait Directed[V] extends EdgeLike[V] {
    def from: V

    def to: V
}

trait Undirected[V] extends EdgeLike[V] {
    def vertex: V

    def other(v: V): Option[V]
}

/**
 * Object to provide non-instance methods for an ordered edge.
 */
object BaseOrderedEdge {
    def compare[V, E: Ordering](x: Edge[V, E], y: Edge[V, E]): Int = implicitly[Ordering[E]].compare(x.attribute, y.attribute)

}