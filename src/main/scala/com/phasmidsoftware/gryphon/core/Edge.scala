package com.phasmidsoftware.gryphon.core

trait Edge[V, E] extends EdgeLike[V] {
    def attribute: E

    def vertices: (V, V)
}

abstract class UndirectedEdge[V, E](v1: V, v2: V, val attribute: E) extends Edge[V, E] with Undirected[V] {
    def vertex: V = v1 // NOTE could equally be v2 but note that method vertices relies on this being deterministic.

    def other(v: V): Option[V] = if (v == v1) Some(v2) else if (v == v2) Some(v1) else None

    def vertices: (V, V) = vertex -> other(vertex).get // NOTE this is guaranteed to have a value.
}

abstract class DirectedEdge[V, E](val from: V, val to: V, val attribute: E) extends Edge[V, E] with Directed[V]

abstract class UndirectedOrderableEdge[V, E: Ordering](v1: V, v2: V, override val attribute: E) extends UndirectedEdge[V, E](v1, v2, attribute) with Ordering[UndirectedOrderableEdge[V, E]] {
    def compare(x: UndirectedOrderableEdge[V, E], y: UndirectedOrderableEdge[V, E]): Int = implicitly[Ordering[E]].compare(x.attribute, y.attribute)
}

abstract class DirectedOrderableEdge[V, E: Ordering](override val from: V, override val to: V, override val attribute: E) extends DirectedEdge[V, E](from, to, attribute) with Ordering[DirectedOrderableEdge[V, E]] {
    def compare(x: DirectedOrderableEdge[V, E], y: DirectedOrderableEdge[V, E]): Int = implicitly[Ordering[E]].compare(x.attribute, y.attribute)
}

trait EdgeLike[V]

trait Directed[V] extends EdgeLike[V] {
    def from: V

    def to: V
}

trait Undirected[V] extends EdgeLike[V] {
    def vertex: V

    def other(v: V): Option[V]
}