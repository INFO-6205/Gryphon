package com.phasmidsoftware.gryphon.core

/**
 * Trait to model the behavior of a graph.
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 * @tparam X the type of edge which connects two vertices. A sub-type of EdgeLike[V].
 *
 */
trait Graph[V, E, X <: Edge[V, E]] extends GraphLike[V, E] {

    /**
     * (abstract) Yield an iterable of edges, of type X.
     *
     * @return an Iterable[X].
     */
    val edges: Iterable[X]

    /**
     * (abstract) The vertex map.
     */
    val vertexMap: VertexMap[V, X]

    /**
     * Yield an iterable of vertices of type V.
     *
     * @return an Iterable[V].
     */
    val vertices: Iterable[V] = vertexMap.keys

    /**
     * Yield an iterable of edge attributes of type E.
     */
    lazy val edgeAttributes: Iterable[E] = edges.map(_.attribute)

    /**
     * (abstract) Method to create a new Graph which includes the given edge.
     *
     * @param x the edge to add.
     * @return Graph[V, E, X].
     */
    def addEdge(x: X): Graph[V, E, X]

    /**
     * Method to run depth-first-search on this Graph.
     *
     * @param visitor the visitor, of type Visitor[V, J].
     * @param v       the starting vertex.
     * @tparam J the journal type.
     * @return a new Visitor[V, J].
     */
    def dfs[J](visitor: Visitor[V, J])(v: V): Visitor[V, J] = vertexMap.dfs(visitor)(v)
}

abstract class AbstractGraph[V, E, X <: Edge[V, E]](val __vertexMap: VertexMap[V, X]) extends Graph[V, E, X] {
    /**
     * Method to add a vertex of (key) type V to this graph.
     * The vertex will have degree of zero.
     *
     * @param v the (key) attribute of the result.
     * @return a new AbstractGraph[V, E, X].
     */
    def addVertex(v: V): AbstractGraph[V, E, X] = unit(__vertexMap addVertex v)

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
    def unit(vertexMap: VertexMap[V, X]): AbstractGraph[V, E, X]
}

abstract class AbstractDirectedGraph[V, E](val _vertexMap: VertexMap[V, DirectedEdge[V, E]]) extends AbstractGraph[V, E, DirectedEdge[V, E]](_vertexMap) {
    /**
     * Method to yield all edges of this AbstractDirectedGraph.
     *
     * @return an Iterable of DirectedEdge[V, E].
     */
    val edges: Iterable[DirectedEdge[V, E]] = allAdjacencies.xs

    /**
     * Method to create a new AbstractGraph which includes the edge x.
     *
     * TESTME
     *
     * @param x an edge to be added to this AbstractDirectedGraph.
     * @return a new AbstractGraph which also includes x.
     */
    def addEdge(x: DirectedEdge[V, E]): AbstractGraph[V, E, DirectedEdge[V, E]] =
        unit(_vertexMap.addEdge(x._from, x).addVertex(x._to))
}

abstract class AbstractUndirectedGraph[V, E](val _vertexMap: VertexMap[V, UndirectedEdge[V, E]]) extends AbstractGraph[V, E, UndirectedEdge[V, E]](_vertexMap) {
    /**
     * Method to yield all edges of this AbstractUndirectedGraph.
     *
     * @return an Iterable of UndirectedEdge[V, E].
     */
    val edges: Iterable[UndirectedEdge[V, E]] = allAdjacencies.xs.distinct

    /**
     * Method to create a new AbstractGraph which includes the edge x.
     *
     * @param x an edge to be added to this AbstractDirectedGraph.
     * @return a new AbstractGraph which also includes x.
     */
    def addEdge(x: UndirectedEdge[V, E]): AbstractGraph[V, E, UndirectedEdge[V, E]] = {
        val (v, w) = x.vertices
        unit(_vertexMap.addEdge(v, x).addEdge(w, x).addVertex(w))
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
case class DirectedGraph[V, E](vertexMap: VertexMap[V, DirectedEdge[V, E]]) extends AbstractDirectedGraph[V, E](vertexMap) {

    /**
     * Method to create a new DirectedGraph from a given vertex map.
     *
     * @param vertexMap the vertex map.
     * @return a new DirectedGraph[V, E].
     */
    def unit(vertexMap: VertexMap[V, DirectedEdge[V, E]]): AbstractGraph[V, E, DirectedEdge[V, E]] = DirectedGraph(vertexMap)
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
    def apply[V, E]: DirectedGraph[V, E] = new DirectedGraph(UnorderedVertexMap.empty)

    /**
     * Method to construct a new empty directed graph with orderable vertex-type.
     *
     * TESTME
     *
     * @tparam V the (key) vertex-attribute type.
     *           Requires implicit evidence of Ordering[V].
     * @tparam E the edge-attribute type.
     * @return an empty UndirectedGraph[V, E].
     */
    def createOrdered[V: Ordering, E]: DirectedGraph[V, E] = new DirectedGraph(OrderedVertexMap.empty[V, DirectedEdge[V, E]])
}


/**
 * A class to represent an undirectedGraph.
 *
 * @param vertexMap its vertex map, i.e. the map of adjacency lists.
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
case class UndirectedGraph[V, E](vertexMap: VertexMap[V, UndirectedEdge[V, E]]) extends AbstractUndirectedGraph[V, E](vertexMap) {

    /**
     * Method to create a new UndirectedGraph from a given vertex map.
     *
     * @param vertexMap the vertex map.
     * @return a new UndirectedGraph[V, E].
     */
    def unit(vertexMap: VertexMap[V, UndirectedEdge[V, E]]): AbstractGraph[V, E, UndirectedEdge[V, E]] = UndirectedGraph(vertexMap)
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
    def apply[V, E]: UndirectedGraph[V, E] = new UndirectedGraph(UnorderedVertexMap.empty[V, UndirectedEdge[V, E]])

    /**
     * Method to construct a new empty undirected graph with orderable vertex-type.
     *
     * @tparam V the (key) vertex-attribute type.
     *           Requires implicit evidence of Ordering[V].
     * @tparam E the edge-attribute type.
     * @return an empty UndirectedGraph[V, E].
     */
    def createOrdered[V: Ordering, E]: UndirectedGraph[V, E] = new UndirectedGraph(OrderedVertexMap.empty[V, UndirectedEdge[V, E]])
}