package com.phasmidsoftware.gryphon.core

/**
 * Trait to model the behavior of a graph.
 *
 * The attribute type for a Graph is String.
 * The edge and vertex attributes are whatever you like (E and V respectively -- see below).
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 * @tparam X the type of edge which connects two vertices. A sub-type of Edge[V,E].
 *
 */
trait Graph[V, E, X <: Edge[V, E]] extends GraphLike[V, E] with Attributed[String] with Traversable[V] {

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
     * Method to add a vertex of (key) type V to this graph.
     * The vertex will have degree of zero.
     *
     * @param v the (key) attribute of the result.
     * @return a new AbstractGraph[V, E, X].
     */
    def addVertex(v: V): AbstractGraph[V, E, X]

    /**
     * Method to run depth-first-search on this Graph.
     *
     * @param visitor the visitor, of type Visitor[V, J].
     * @param v       the starting vertex.
     * @tparam J the journal type.
     * @return a new Visitor[V, J].
     */
    def dfs[J](visitor: Visitor[V, J])(v: V): Visitor[V, J] = vertexMap.dfs(visitor)(v)

    /**
     * Method to run breadth-first-search on this Graph.
     *
     * @param visitor the visitor, of type Visitor[V, J].
     *                Note that only "pre" events are recorded by this Visitor.
     * @param v       the starting vertex.
     * @tparam J the journal type.
     * @return a new Visitor[V, J].
     */
    def bfs[J](visitor: Visitor[V, J])(v: V): Visitor[V, J] = vertexMap.bfs(visitor)(v)

    /**
     * Method to run breadth-first-search with a mutable queue on this Graph.
     *
     * @param visitor the visitor, of type Visitor[V, J].
     * @param v       the starting vertex.
     * @tparam J the journal type.
     * @tparam Q the type of the mutable queue for navigating this Traversable.
     *           Requires implicit evidence of MutableQueueable[Q, V].
     * @return a new Visitor[V, J].
     */
    def bfsMutable[J, Q](visitor: Visitor[V, J])(v: V)(implicit ev: MutableQueueable[Q, V]): Visitor[V, J] = vertexMap.bfsMutable[J, Q](visitor)(v)
}

/**
 * Trait to define the behavior of a graph-like object.
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
trait GraphLike[V, E]

/**
 * Trait to define the behavior of a directed graph.
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 * @tparam X the type of edge which connects two vertices. A sub-type of DirectedEdge[V,E].
 */
trait DirectedGraph[V, E, X <: DirectedEdge[V, E]] extends Graph[V, E, X]

/**
 * Trait to define the behavior of an undirected graph.
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 * @tparam X the type of edge which connects two vertices. A sub-type of UndirectedEdge[V,E].
 */
trait UndirectedGraph[V, E, X <: UndirectedEdge[V, E]] extends Graph[V, E, X]

/**
 * Abstract class to represent a graph.
 *
 * The attribute type for a Graph is always String. CONSIDER relaxing this.
 * The edge and vertex attributes are whatever you like (E and V respectively -- see below).
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 * @tparam X the type of edge which connects two vertices. A sub-type of Edge[V,E].
 *
 */
abstract class AbstractGraph[V, E, X <: Edge[V, E]](val __description: String, val __vertexMap: VertexMap[V, X]) extends Graph[V, E, X] {

    /**
     * Yields the description of this Graph.
     */
    val attribute: String = __description

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

/**
 * Abstract class to represent a directed graph.
 *
 * The attribute type for a Graph is always String. CONSIDER relaxing this.
 * The edge and vertex attributes are whatever you like (E and V respectively -- see below).
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 *
 */
abstract class AbstractDirectedGraph[V, E](val _description: String, val _vertexMap: VertexMap[V, DirectedEdge[V, E]]) extends AbstractGraph[V, E, DirectedEdge[V, E]](_description, _vertexMap) with DirectedGraph[V, E, DirectedEdge[V, E]] {
    /**
     * Method to yield all edges of this AbstractDirectedGraph.
     *
     * @return an Iterable of DirectedEdgeCase[V, E].
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
        unit(_vertexMap.addEdge(x.from, x).addVertex(x.to))
}

/**
 * Abstract class to represent an undirected graph.
 *
 * The attribute type for a Graph is always String. CONSIDER relaxing this.
 * The edge and vertex attributes are whatever you like (E and V respectively -- see below).
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 *
 */
abstract class AbstractUndirectedGraph[V, E](val _description: String, val _vertexMap: VertexMap[V, UndirectedEdge[V, E]]) extends AbstractGraph[V, E, UndirectedEdge[V, E]](_description, _vertexMap) with UndirectedGraph[V, E, UndirectedEdge[V, E]] {
    /**
     * Method to yield all edges of this AbstractUndirectedGraph.
     *
     * @return an Iterable of UndirectedEdgeCase[V, E].
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
 * A case class to represent a DirectedGraph.
 *
 * @param vertexMap its vertex map, i.e. the map of adjacency lists.
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
case class DirectedGraphCase[V, E](description: String, vertexMap: VertexMap[V, DirectedEdge[V, E]]) extends AbstractDirectedGraph[V, E](description, vertexMap) with DirectedGraph[V, E, DirectedEdge[V, E]] {

    /**
     * Method to create a new DirectedGraphCase from a given vertex map.
     *
     * @param vertexMap the vertex map.
     * @return a new DirectedGraphCase[V, E].
     */
    def unit(vertexMap: VertexMap[V, DirectedEdge[V, E]]): AbstractGraph[V, E, DirectedEdge[V, E]] = DirectedGraphCase(description, vertexMap)
}

/**
 * A case class to represent an undirectedGraph.
 *
 * @param vertexMap its vertex map, i.e. the map of adjacency lists.
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
case class UndirectedGraphCase[V, E](description: String, vertexMap: VertexMap[V, UndirectedEdge[V, E]]) extends AbstractUndirectedGraph[V, E](description, vertexMap) with UndirectedGraph[V, E, UndirectedEdge[V, E]] {

    /**
     * Method to create a new UndirectedGraphCase from a given vertex map.
     *
     * @param vertexMap the vertex map.
     * @return a new UndirectedGraphCase[V, E].
     */
    def unit(vertexMap: VertexMap[V, UndirectedEdge[V, E]]): AbstractGraph[V, E, UndirectedEdge[V, E]] = UndirectedGraphCase(description, vertexMap)
}

/**
 * Object to provide non-instance directed graph properties.
 */
object DirectedGraph {
    /**
     * Method to construct a new empty directed graph.
     *
     * @tparam V the (key) vertex-attribute type.
     * @tparam E the edge-attribute type.
     * @return an empty DirectedGraphCase[V, E].
     */
    def apply[V, E](description: String): DirectedGraph[V, E, DirectedEdge[V, E]] = new DirectedGraphCase[V, E](description, UnorderedVertexMap.empty[V, DirectedEdge[V, E]])

    /**
     * Method to construct a new empty directed graph with orderable vertex-type.
     *
     * TESTME
     *
     * @tparam V the (key) vertex-attribute type.
     *           Requires implicit evidence of Ordering[V].
     * @tparam E the edge-attribute type.
     * @return an empty UndirectedGraphCase[V, E].
     */
    def createOrdered[V: Ordering, E](description: String): DirectedGraph[V, E, DirectedEdge[V, E]] = DirectedGraphCase(description, OrderedVertexMap.empty[V, DirectedEdge[V, E]])
}

/**
 * Object to provide non-instance undirected graph properties.
 */
object UndirectedGraph {
    /**
     * Method to construct a new empty undirected graph.
     *
     * @tparam V the (key) vertex-attribute type.
     * @tparam E the edge-attribute type.
     * @return an empty UndirectedGraphCase[V, E].
     */
    def apply[V, E](description: String): UndirectedGraph[V, E, UndirectedEdge[V, E]] = UndirectedGraphCase(description, UnorderedVertexMap.empty[V, UndirectedEdge[V, E]])

    /**
     * Method to construct a new empty undirected graph with orderable vertex-type.
     *
     * @tparam V the (key) vertex-attribute type.
     *           Requires implicit evidence of Ordering[V].
     * @tparam E the edge-attribute type.
     * @return an empty UndirectedGraphCase[V, E].
     */
    def createOrdered[V: Ordering, E](description: String): UndirectedGraph[V, E, UndirectedEdge[V, E]] = UndirectedGraphCase(description, OrderedVertexMap.empty[V, UndirectedEdge[V, E]])

}
