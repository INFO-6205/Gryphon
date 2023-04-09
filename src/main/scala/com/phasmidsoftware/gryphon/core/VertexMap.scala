package com.phasmidsoftware.gryphon.core

import scala.collection.immutable.{HashMap, TreeMap}

/**
 * Trait to define the behavior of a "vertex map," i.e. the set of adjacency lists for a graph.
 *
 * @tparam V the (key) vertex-type of a graph.
 * @tparam X the edge-type of a graph. A sub-type of EdgeLike[V].
 */
trait VertexMap[V, X <: EdgeLike[V]] {
    /**
     * the vertex-type values, i.e. the keys, of this VertexMap.
     */
    val keys: Iterable[V]

    /**
     * the Vertex[V, X] values of this VertexMap.
     */
    val values: Iterable[Vertex[V, X]]

    /**
     * Method to add a vertex to this VertexMap.
     *
     * @param v the (key) value of the vertex to be added.
     * @return a new VertexMap which includes all the original entries of <code>this</code> plus <code>v</code>.
     */
    def addVertex(v: V): VertexMap[V, X]

    /**
     * Method to add an edge to this VertexMap.
     *
     * @param v the (key) value of the vertex whose adjacency list we are adding to.
     * @param x the edge to be added to the adjacency list.
     * @return a new VertexMap which includes all the original entries of <code>this</code> plus <code>v -> x</code>.
     */
    def addEdge(v: V, x: X): VertexMap[V, X]
}

/**
 * Case class to represent an ordered VertexMap.
 * The ordering is based on the key (V) type.
 *
 * @param map a TreeMap of V -> Vertex[V, X].
 * @tparam V the (key) vertex-attribute type.
 *           Requires implicit evidence of type Ordering[V].
 * @tparam X the type of edge which connects two vertices. A sub-type of EdgeLike[V].
 */
case class OrderedVertexMap[V: Ordering, X <: EdgeLike[V]](map: TreeMap[V, Vertex[V, X]]) extends BaseVertexMap[V, X](map) {

    /**
     * Method to construct a new OrderedVertexMap from the given map.
     *
     * @param map a TreeMap. If it is not a TreeMap, it will be converted to one.
     * @return a new OrderedVertexMap[V, X].
     */
    def unit(map: Map[V, Vertex[V, X]]): OrderedVertexMap[V, X] = OrderedVertexMap[V, X](map.to(TreeMap))
}

object OrderedVertexMap {
    /**
     *
     * @tparam V the (key) vertex-attribute type.
     *           Requires implicit evidence of type Ordering[V].
     * @tparam X the type of edge which connects two vertices. A sub-type of EdgeLike[V].
     * @return an empty OrderedVertexMap[V, X].
     */
    def empty[V: Ordering, X <: EdgeLike[V]]: VertexMap[V, X] = OrderedVertexMap(TreeMap.empty[V, Vertex[V, X]])
}


case class UnorderedVertexMap[V, X <: EdgeLike[V]](map: HashMap[V, Vertex[V, X]]) extends BaseVertexMap[V, X](map) {

    /**
     * Method to construct a new UnorderedVertexMap from the given map.
     *
     * @param map a HashMap. If it is not a HashMap, it will be converted to one.
     * @return a new UnorderedVertexMap[V, X].
     */
    def unit(map: Map[V, Vertex[V, X]]): UnorderedVertexMap[V, X] = UnorderedVertexMap[V, X](map.to(HashMap))
}

object UnorderedVertexMap {
    def empty[V, X <: EdgeLike[V]]: VertexMap[V, X] = UnorderedVertexMap(HashMap.empty[V, Vertex[V, X]])
}

abstract class BaseVertexMap[V, X <: EdgeLike[V]](val _map: Map[V, Vertex[V, X]]) extends VertexMap[V, X] {

    /**
     * Method to add a vertex of (key) type V to this graph.
     * The vertex will have degree of zero.
     *
     * @param v the (key) attribute of the result.
     * @return a new AbstractGraph[V, E, X].
     */
    def addVertex(v: V): VertexMap[V, X] = unit(_map + (v -> Vertex.empty[V, X](v)))

    /**
     * Method to add an edge to this VertexMap.
     *
     * @param v the (key) value of the vertex whose adjacency list we are adding to.
     * @param x the edge to be added to the adjacency list.
     * @return a new VertexMap which includes all the original entries of <code>this</code> plus <code>v -> x</code>.
     */
    def addEdge(v: V, x: X): VertexMap[V, X] = {
        val z: Map[V, Vertex[V, X]] = _map.get(v) match {
            case Some(vv) => (_map - v) + (v -> (vv addEdge x))
            case None => _map + (v -> (Vertex.empty(v) addEdge x))
        }
        unit(z)
    }

    /**
     * the vertex-type values, i.e. the keys, of this VertexMap.
     */
    val keys: Iterable[V] = _map.keys

    /**
     * the Vertex[V, X] values of this VertexMap.
     */
    val values: Iterable[Vertex[V, X]] = _map.values

    /**
     * (abstract) Method to construct a new VertexMap from the given map.
     *
     * @param map a Map (might be TreeMap or HashMap).
     * @return a new VertexMap[V, X].
     */
    def unit(map: Map[V, Vertex[V, X]]): VertexMap[V, X]
}
