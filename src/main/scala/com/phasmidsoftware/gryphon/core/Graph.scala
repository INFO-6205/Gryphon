package com.phasmidsoftware.gryphon.core

/**
 * Trait to model the behavior of a graph.
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 * @tparam X the type of edge which connects two vertices.
 */
trait Graph[V, E, X <: Edge[V, E]] extends GraphLike[V, E] {

    /**
     * Yield an iterable of edges.
     *
     * @return an Iterable[X].
     */
    val edges: Iterable[X]

    /**
     * The vertex map, a Map of V and Vertex[V, X].
     */
    val vertexMap: Map[V, Vertex[V, X]]

    /**
     * Yield an iterable of vertices.
     *
     * @return an Iterable[V].
     */
    val vertices: Iterable[V] = vertexMap.keys

    /**
     * (abstract) Method to create a new Graph which includes the given edge.
     *
     * @param x the edge to add.
     * @return Graph[V, E, X].
     */
    def addEdge(x: X): Graph[V, E, X]
}

abstract class AbstractGraph[V, E, X <: Edge[V, E]](val __vertexMap: Map[V, Vertex[V, X]]) extends Graph[V, E, X] {
    /**
     * Method to add a vertex of (key) type V to this graph.
     * The vertex will have degree of zero.
     *
     * @param v the (key) attribute of the result.
     * @return a new AbstractGraph[V, E, X].
     */
    def addVertex(v: V): AbstractGraph[V, E, X] = unit(__vertexMap + (v -> Vertex.empty[V, X](v)))

    /**
     * Method to yield the concatenation of the all the adjacency lists.
     *
     * @return AdjacencyList[X]
     */
    def allAdjacencies: AdjacencyList[X] = __vertexMap.values.foldLeft(AdjacencyList.empty[X])(_ ++ _.adjacent)

    /**
     * (abstract) Method to create a new AbstractGraph from a given vertex map.
     *
     * @param vertexMap the vertex map.
     * @return a new AbstractGraph[V, E].
     */
    def unit(vertexMap: Map[V, Vertex[V, X]]): AbstractGraph[V, E, X]
}

/**
 * Companion object to AbstractGraph.
 */
object AbstractGraph {
    /**
     * Method to add an edge to a vertex's adjacency map.
     *
     * @param vertexMap the adjacency map of this graph: it's of type <code>Map of V, Vertex[V, X]</code>.
     * @param v         the vertex whose adjacency list is to be updated.
     * @param x         the edge to be added to x's adjacency list.
     * @return a new Map of V, Vertex[V, X].
     */
    def addXatV[V, E, X <: Edge[V, E]](vertexMap: Map[V, Vertex[V, X]])(v: V, x: X): Map[V, Vertex[V, X]] = vertexMap.-(v) + (v -> vertexMap(v).addEdge(x))
}

abstract class AbstractDirectedGraph[V, E](val _vertexMap: Map[V, Vertex[V, BaseDirectedEdge[V, E]]]) extends AbstractGraph[V, E, BaseDirectedEdge[V, E]](_vertexMap) {
    /**
     * Method to yield all edges of this AbstractDirectedGraph.
     *
     * @return an Iterable of BaseDirectedEdge[V, E].
     */
    val edges: Iterable[BaseDirectedEdge[V, E]] = allAdjacencies.xs

    /**
     * Method to create a new AbstractGraph which includes the edge x.
     *
     * @param x an edge to be added to this AbstractDirectedGraph.
     * @return a new AbstractGraph which also includes x.
     */
    def addEdge(x: BaseDirectedEdge[V, E]): AbstractGraph[V, E, BaseDirectedEdge[V, E]] = unit(AbstractGraph.addXatV[V, E, BaseDirectedEdge[V, E]](vertexMap)(x.from, x))
}

abstract class AbstractUndirectedGraph[V, E](val _vertexMap: Map[V, Vertex[V, BaseUndirectedEdge[V, E]]]) extends AbstractGraph[V, E, BaseUndirectedEdge[V, E]](_vertexMap) {
    /**
     * Method to yield all edges of this AbstractUndirectedGraph.
     *
     * @return an Iterable of BaseUndirectedEdge[V, E].
     */
    val edges: Iterable[BaseUndirectedEdge[V, E]] = allAdjacencies.xs.distinct

    /**
     * Method to create a new AbstractGraph which includes the edge x.
     *
     * @param x an edge to be added to this AbstractDirectedGraph.
     * @return a new AbstractGraph which also includes x.
     */
    def addEdge(x: BaseUndirectedEdge[V, E]): AbstractGraph[V, E, BaseUndirectedEdge[V, E]] = {
        val (v, w) = x.vertices
        val f = AbstractGraph.addXatV[V, E, BaseUndirectedEdge[V, E]] _
        unit(f(f(vertexMap)(v, x))(w, x))
    }
}

/**
 * Trait to define the behavior of a graph-like object.
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
trait GraphLike[V, E]

/**
 * A class to represent a DirectedGraph.
 *
 * @param vertexMap its vertex map, i.e. the map of adjacency lists.
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
case class DirectedGraph[V, E](vertexMap: Map[V, Vertex[V, BaseDirectedEdge[V, E]]]) extends AbstractDirectedGraph[V, E](vertexMap) {

    /**
     * Method to create a new DirectedGraph from a given vertex map.
     *
     * @param vertexMap the vertex map.
     * @return a new DirectedGraph[V, E].
     */
    def unit(vertexMap: Map[V, Vertex[V, BaseDirectedEdge[V, E]]]): AbstractGraph[V, E, BaseDirectedEdge[V, E]] = DirectedGraph(vertexMap)
}

/**
 * Companion object of DirectedGraph.
 */
object DirectedGraph {
    /**
     * Method to construct a new empty directed graph.
     *
     * @tparam V the (key) vertex-attribute type.
     * @tparam E the edge-attribute type.
     * @return an empty DirectedGraph[V, E].
     */
    def apply[V, E]: DirectedGraph[V, E] = new DirectedGraph(Map.empty)
}


/**
 * A class to represent an undirectedGraph.
 *
 * @param vertexMap its vertex map, i.e. the map of adjacency lists.
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
case class UndirectedGraph[V, E](vertexMap: Map[V, Vertex[V, BaseUndirectedEdge[V, E]]]) extends AbstractUndirectedGraph[V, E](vertexMap) {

    /**
     * Method to create a new UndirectedGraph from a given vertex map.
     *
     * @param vertexMap the vertex map.
     * @return a new UndirectedGraph[V, E].
     */
    def unit(vertexMap: Map[V, Vertex[V, BaseUndirectedEdge[V, E]]]): AbstractGraph[V, E, BaseUndirectedEdge[V, E]] = UndirectedGraph(vertexMap)
}

/**
 * Companion object of UndirectedGraph.
 */
object UndirectedGraph {
    /**
     * Method to construct a new empty undirected graph.
     *
     * @tparam V the (key) vertex-attribute type.
     * @tparam E the edge-attribute type.
     * @return an empty UndirectedGraph[V, E].
     */
    def apply[V, E]: UndirectedGraph[V, E] = new UndirectedGraph(Map.empty)
}