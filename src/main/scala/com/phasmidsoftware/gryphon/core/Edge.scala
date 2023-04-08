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

    /**
     * The two vertices of this Edge.
     */
    val vertices: (V, V)
}

/**
 * Abstract base class to represent an undirected edge.
 *
 * @param _v1        a vertex.
 * @param _v2        the other vertex.
 * @param _attribute the edge's attribute.
 * @tparam V the Vertex key type, i.e. the type of its attribute. Must provide evidence of Ordering.
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
    def other(v: V): Option[V] = if (v == _v1) Some(_v2) else if (v == _v2) Some(_v1) else None

    /**
     * Method to get the two vertices of this edge in some deterministic order, based on the implicit value of Ordering[V].
     */
    val vertices: (V, V) = {
        val v = if (implicitly[Ordering[V]].compare(_v1, _v2) <= 0) _v1 else _v2
        vertex -> other(v).get // NOTE this is guaranteed to have a value.
    }
}

abstract class BaseDirectedEdge[V, E](val _from: V, val _to: V, val _attribute: E) extends Edge[V, E] with Directed[V] {
    val vertices: (V, V) = _from -> _to
}

abstract class BaseUndirectedOrderedEdge[V: Ordering, E: Ordering](_v1: V, _v2: V, override val _attribute: E) extends BaseUndirectedEdge[V, E](_v1, _v2, _attribute) with Ordering[BaseUndirectedOrderedEdge[V, E]] {
    def compare(x: BaseUndirectedOrderedEdge[V, E], y: BaseUndirectedOrderedEdge[V, E]): Int = implicitly[Ordering[E]].compare(x.attribute, y.attribute)
}

abstract class BaseDirectedOrderedEdge[V, E: Ordering](override val _from: V, override val _to: V, override val _attribute: E) extends BaseDirectedEdge[V, E](_from, _to, _attribute) with Ordering[BaseDirectedOrderedEdge[V, E]] {
    def compare(x: BaseDirectedOrderedEdge[V, E], y: BaseDirectedOrderedEdge[V, E]): Int = implicitly[Ordering[E]].compare(x.attribute, y.attribute)
}

case class DirectedEdge[V, E](from: V, to: V, attribute: E) extends BaseDirectedEdge[V, E](from, to, attribute)

case class DirectedOrderedEdge[V, E: Ordering](from: V, to: V, attribute: E) extends BaseDirectedOrderedEdge[V, E](from, to, attribute)

case class UndirectedEdge[V: Ordering, E](from: V, to: V, attribute: E) extends BaseUndirectedEdge[V, E](from, to, attribute)

case class UndirectedOrderedEdge[V: Ordering, E: Ordering](from: V, to: V, attribute: E) extends BaseUndirectedOrderedEdge[V, E](from, to, attribute)

trait EdgeLike[V]

trait Directed[V] extends EdgeLike[V] {
    def from: V

    def to: V
}

trait Undirected[V] extends EdgeLike[V] {
    def vertex: V

    def other(v: V): Option[V]
}