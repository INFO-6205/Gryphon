package com.phasmidsoftware.gryphon.core

import scala.collection.immutable.{HashMap, TreeMap}

trait VertexMap[V, X <: EdgeLike[V]] {
    def keys: Iterable[V]

    def values: Iterable[Vertex[V, X]]

    def addVertex(v: V): VertexMap[V, X]

    def addEdge(v: V, x: X): VertexMap[V, X]

}

// TODO rename this
case class OrderedVertexMap1[V: Ordering, X <: EdgeLike[V]](map: TreeMap[V, Vertex[V, X]]) extends BaseVertexMap[V, X](map) {
    def +(kv: (V, Vertex[V, X])): OrderedVertexMap1[V, X] = unit((_map + kv).to(TreeMap))

    def unit(map: Map[V, Vertex[V, X]]): OrderedVertexMap1[V, X] = OrderedVertexMap1[V, X](map.to(TreeMap))
}

object OrderedVertexMap1 {
    def empty[V: Ordering, X <: EdgeLike[V]]: VertexMap[V, X] = OrderedVertexMap1(TreeMap.empty[V, Vertex[V, X]])
}


case class UnorderedVertexMap[V, X <: EdgeLike[V]](map: HashMap[V, Vertex[V, X]]) extends BaseVertexMap[V, X](map) {
    def +(kv: (V, Vertex[V, X])): UnorderedVertexMap[V, X] = unit((_map + kv).to(HashMap))

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

    def addEdge(v: V, x: X): VertexMap[V, X] = {
        val z: Map[V, Vertex[V, X]] = _map.get(v) match {
            case Some(vv) => (_map - v) + (v -> (vv addEdge x))
            case None => _map + (v -> (Vertex.empty(v) addEdge x))
        }
        unit(z)
    }

    def keys: Iterable[V] = _map.keys

    def values: Iterable[Vertex[V, X]] = _map.values

    def unit(map: Map[V, Vertex[V, X]]): VertexMap[V, X]
}
