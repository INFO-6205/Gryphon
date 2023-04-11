package com.phasmidsoftware.gryphon.core

import scala.collection.immutable.{HashMap, TreeMap}

/**
 * Trait to define the behavior of a "vertex map," i.e. the set of adjacency lists for a graph.
 *
 * There are two distinct types of VertexMap:
 * <ol>
 * <li>Those that can be ordered according to type V (these will use a TreeMap)</li>
 * <li>Those that can't be ordered according to type V (these will use a HashMap)</li>
 * </ol>
 *
 * @tparam V the (key) vertex-type of a graph.
 * @tparam X the edge-type of a graph. A sub-type of EdgeLike[V].
 */
trait VertexMap[V, X <: EdgeLike[V]] {
    /**
     * Method to get the AdjacencyList for vertex with key (attribute) v, if there is one.
     *
     * @param v the key (attribute) of the vertex whose adjacency list we require.
     * @return an Option of AdjacencyList[X].
     */
    def optAdjacencyList(v: V): Option[AdjacencyList[X]]

    /**
     * the vertex-type values, i.e. the keys, of this VertexMap.
     */
    val keys: Iterable[V]

    /**
     * the Vertex[V, X] values of this VertexMap.
     */
    val values: Iterable[Vertex[V, X]]

    /**
     * the X values of this VertexMap.
     */
    val edges: Iterable[X]

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

    def dfs[J](visitor: Visitor[V, J])(v: V): Visitor[V, J] = {
        val pre = visitor.visitPre(v)
        val zo: Option[Visitor[V, J]] = optAdjacencyList(v) map {
            xa =>
                xa.xs.foldLeft(pre) {
                    (b, x) =>
                        x.other(v) match {
                            case Some(z) => dfs(b)(z)
                            case None => throw GraphException(s"Logic error 1: dfs($v): x: $x")
                        }
                }
        }
        zo map (z => z.visitPost(v)) getOrElse pre.visitPost(v) // TODO fix this.
    }
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

/**
 * Companion object to OrderedVertexMap.
 */
object OrderedVertexMap {
    /**
     * Method to yield an empty OrderedVertexMap.
     *
     * @tparam V the (key) vertex-attribute type.
     *           Requires implicit evidence of type Ordering[V].
     * @tparam X the type of edge which connects two vertices. A sub-type of EdgeLike[V].
     * @return an empty OrderedVertexMap[V, X].
     */
    def empty[V: Ordering, X <: EdgeLike[V]]: VertexMap[V, X] = OrderedVertexMap(TreeMap.empty[V, Vertex[V, X]])
}


/**
 * Case class to represent an unordered VertexMap.
 *
 * @param map a HashMap of V -> Vertex[V, X].
 * @tparam V the (key) vertex-attribute type.
 * @tparam X the type of edge which connects two vertices. A sub-type of EdgeLike[V].
 */
case class UnorderedVertexMap[V, X <: EdgeLike[V]](map: HashMap[V, Vertex[V, X]]) extends BaseVertexMap[V, X](map) {

    /**
     * Method to construct a new UnorderedVertexMap from the given map.
     *
     * @param map a HashMap. If it is not a HashMap, it will be converted to one.
     * @return a new UnorderedVertexMap[V, X].
     */
    def unit(map: Map[V, Vertex[V, X]]): UnorderedVertexMap[V, X] = UnorderedVertexMap[V, X](map.to(HashMap))
}

/**
 * Companion object to UnorderedVertexMap.
 */
object UnorderedVertexMap {
    /**
     * Method to yield an empty UnorderedVertexMap.
     *
     * @tparam V the (key) vertex-attribute type.
     * @tparam X the type of edge which connects two vertices. A sub-type of EdgeLike[V].
     * @return an empty UnorderedVertexMap[V, X].
     */
    def empty[V, X <: EdgeLike[V]]: VertexMap[V, X] = UnorderedVertexMap(HashMap.empty[V, Vertex[V, X]])
}

/**
 * Abstract base class to define general VertexMap properties.
 *
 * @param _map a Map of V -> Vertex[V, X].
 * @tparam V the (key) vertex-attribute type.
 * @tparam X the type of edge which connects two vertices. A sub-type of EdgeLike[V].
 */
abstract class BaseVertexMap[V, X <: EdgeLike[V]](val _map: Map[V, Vertex[V, X]]) extends VertexMap[V, X] {

    /**
     * Method to get the AdjacencyList for vertex with key (attribute) v, if there is one.
     *
     * @param v the key (attribute) of the vertex whose adjacency list we require.
     * @return an Option of AdjacencyList[X].
     */
    def optAdjacencyList(v: V): Option[AdjacencyList[X]] = _map.get(v) map (_.adjacent)

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
    def addEdge(v: V, x: X): VertexMap[V, X] = unit(
        _map.get(v) match {
            case Some(vv) => buildMap(_map - v, v, x, vv)
            case None => buildMap(_map, v, x, Vertex.empty(v))
        }
    )

    /**
     * the vertex-type values, i.e. the keys, of this VertexMap.
     */
    val keys: Iterable[V] = _map.keys

    /**
     * the Vertex[V, X] values of this VertexMap.
     */
    val values: Iterable[Vertex[V, X]] = _map.values

    /**
     * the X values of this VertexMap.
     */
    val edges: Iterable[X] = _map.values.flatMap(_.adjacent.xs)

    /**
     * (abstract) Method to construct a new VertexMap from the given map.
     *
     * @param map a Map (might be TreeMap or HashMap).
     * @return a new VertexMap[V, X].
     */
    def unit(map: Map[V, Vertex[V, X]]): VertexMap[V, X]

    private def buildMap(base: Map[V, Vertex[V, X]], v: V, x: X, vv: Vertex[V, X]) = base + (v -> (vv addEdge x))
}
