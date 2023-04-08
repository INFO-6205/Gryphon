package com.phasmidsoftware.gryphon.core

/**
 * Trait to model the behavior of a Vertex.
 *
 * @tparam V the key (attribute) type of this Vertex.
 * @tparam X the "edge" type for the adjacent edges of this Vertex.
 */
trait Vertex[V, X <: EdgeLike[V]] extends VertexLike[V] {
    /**
     * Method to add an edge (x) to this Vertex.
     *
     * @param x the EdgeLike[V] object to be added this Vertex.
     * @return
     */
    def addEdge(x: X): Vertex[V, X]

    /**
     * The adjacency list, an AdjacencyList[X], for this Vertex.
     */
    val adjacent: AdjacencyList[X]

    /**
     * The (out) degree of this Vertex.
     *
     * @return
     */
    def degree: Int = adjacent.size
}

/**
 * Abstract base class to represent an vertex.
 *
 * @tparam V the key (attribute) type of this Vertex.
 * @tparam X the "edge" type for the adjacent edges of this Vertex.
 */
abstract class AbstractVertex[V, X <: EdgeLike[V]] extends Vertex[V, X] {
    /**
     * Method to add an edge to this AbstractVertex.
     *
     * @param x the edge to add.
     * @return a new AbstractVertex which includes the new edge in its adjacency list.
     */
    def addEdge(x: X): Vertex[V, X] = unit(AdjacencyList(x +: adjacent.xs))

    /**
     * Method to construct a new AbstractVertex.
     *
     * @param adjacent an AdjacencyList[Y].
     * @tparam Y the edge-type of the resulting AbstractVertex
     * @return a new AbstractVertex[V, Y].
     */
    def unit[Y <: EdgeLike[V]](adjacent: AdjacencyList[Y]): AbstractVertex[V, Y]
}

/**
 * Trait to model the behavior of a vertex-like object.
 *
 * @tparam V the (covariant) attribute type.
 */
trait VertexLike[+V] extends Attributed[V]
