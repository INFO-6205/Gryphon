package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.applications.mst.TSP.createEdgeFromVertices
import com.phasmidsoftware.gryphon.core._
import com.phasmidsoftware.gryphon.util.LazyPriorityQueue

/**
 * Trait to model the behavior of a minimum spanning tree.
 * This works only for undirected graphs.
 *
 * @tparam V the vertex (key) attribute type.
 * @tparam E the edge type.
 */
trait MST[V, E] {

    def mst: Tree[V, E]

    def total(implicit en: Numeric[E]): E
}

/**
 * Abstract class to implement MST[V, E].
 *
 * @tparam V the vertex (key) attribute type.
 *           Requires implicit evidence of type Ordering[V].
 * @tparam E the edge type.
 *           Requires implicit evidence of type Ordering[E].
 */
abstract class BaseMST[V: Ordering, E: Ordering](_mst: Tree[V, E]) extends MST[V, E] {

    def isCyclic: Boolean = _mst.isCyclic

    def isBipartite: Boolean = _mst.isBipartite
}

/**
 * Implementation class which evaluates the MST using Prim's (lazy) algorithm.
 *
 * @tparam V the vertex (key) attribute type.
 *           Requires implicit evidence of type Ordering[V].
 * @tparam E the edge type.
 *           Requires implicit evidence of type Ordering[E].
 */
case class LazyPrim[V: Ordering, E: Ordering](mst: Tree[V, E]) extends BaseMST[V, E](mst) {

    /**
     * (abstract) Yield an iterable of edges, of type X.
     *
     * @return an Iterable[X].
     */
    val edges: Iterable[UndirectedEdge[V, E]] = mst.edges
    /**
     * (abstract) The vertex map.
     */
    val vertexMap: VertexMap[V, UndirectedEdge[V, E]] = mst.vertexMap

    /**
     * An attribute.
     *
     * @return the value of the attribute, for example, a weight.
     */
    val attribute: String = "LazyPrim"

    def total(implicit en: Numeric[E]): E = edges.map(_.attribute).sum
}

object LazyPrim {
    /**
     * Method to calculate the Minimum Spanning Tree of a graph using the LazyPrim algorithm.
     *
     * @param graph the graph whose MST is required.
     * @return the MST for graph.
     */
    def createFromGraph[V: Ordering, E: Ordering](graph: UndirectedGraph[V, E, UndirectedOrderedEdge[V, E]]): LazyPrim[V, E] = {
        /**
         * Method to yield the candidate edges from the given set of vertices.
         *
         * @param v the most recent vertex to have been added to Prim's tree (or the starting vertex).
         * @param m the current vertex map.
         * @return a set of edges which are candidates to be added to Prim's tree.
         */
        def candidateEdges(v: V, m: OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]): Iterable[UndirectedOrderedEdge[V, E]] =
            graph.vertexMap.adjacentEdgesWithFilter(v)(x => notInTree(v, m, x))

        LazyPrim(TreeCase[V, E](s"MST for graph ${graph.attribute}", doLazyPrim(graph.vertices, candidateEdges)))
    }

    /**
     * Method to calculate the Minimum Spanning Tree of a graph using the LazyPrim algorithm.
     *
     * @param vertices the vertices from which the MST is required
     *                 (potentially all edges between pairs of vertices are considered).
     * @return the MST for graph.
     */
    def createFromVertices[V: Ordering, E: Ordering](vertices: Iterable[V])(implicit d: (V, V) => E): LazyPrim[V, E] = {
        /**
         * Method to yield the candidate edges from the given set of vertices.
         *
         * @param v the most recent vertex to have been added to Prim's tree (or the starting vertex).
         * @param m the current vertex map.
         * @return a set of edges which are candidates to be added to Prim's tree.
         */
        def candidateEdges(v: V, m: OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]): Iterable[UndirectedOrderedEdge[V, E]] =
            for {
                w <- vertices
                vo = implicitly[Ordering[V]]
                eo = implicitly[Ordering[E]]
                x = createEdgeFromVertices(v, w)(vo, eo, d) if notInTree(v, m, x)
            } yield
                x

        LazyPrim(TreeCase[V, E](s"MST for graph from vertices", doLazyPrim(vertices, candidateEdges)))
    }

    private def notInTree[E: Ordering, V: Ordering](v: V, m: OrderedVertexMap[V, UndirectedOrderedEdge[V, E]], x: UndirectedOrderedEdge[V, E]) =
        !m.contains(x.other(v).get)

    private def doLazyPrim[V: Ordering, E: Ordering](vs: Iterable[V], candidateEdges: (V, OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) => Iterable[UndirectedOrderedEdge[V, E]]): OrderedVertexMap[V, UndirectedOrderedEdge[V, E]] = {

        def hasExactlyOneVertexInMap(m: OrderedVertexMap[V, UndirectedOrderedEdge[V, E]])(e: UndirectedOrderedEdge[V, E]) =
            !m.contains(e.vertices._1) ^ !m.contains(e.vertices._2)

        implicit object UndirectedEdgeOrdering extends Ordering[UndirectedOrderedEdge[V, E]] {
            // NOTE that we compare in reverse order.
            def compare(x: UndirectedOrderedEdge[V, E], y: UndirectedOrderedEdge[V, E]): Int = OrderedEdge.compare(y, x)
        }

        // NOTE this is an ordinary PriorityQueue which does not support deletion or changing priority of queues.
        // Thus, this algorithm is the lazy version of Prim's algorithm.
        val pq = LazyPriorityQueue[UndirectedOrderedEdge[V, E]]

        /**
         * Method to grow the tree according to Prim's algorithm.
         * CONSIDER using bfs for this.
         *
         * @param t a tuple consisting of (the vertex most recently added to the tree, the current VertexMap).
         * @return a tuple of (most recently added vertex, new vertex map).
         */
        def grow(t: (Option[V], OrderedVertexMap[V, UndirectedOrderedEdge[V, E]])): (Option[V], OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) =
            t match {
                case (Some(v), m) =>
                    candidateEdges(v, m) foreach (w => pq.addOne(w))
                    // NOTE: because this is the lazy version of Prim, we must still check that other vertex of e is not in the vertex map.
                    pq.conditionalDequeue(hasExactlyOneVertexInMap(m)) match {
                        case Some(e) => m.addEdgeWithVertex(e)
                        case None => None -> m
                    }
                case (None, _) => t
            }

        // Starting at an arbitrary vertex of the graph (we pick the head of the vertices list),
        // gradually build up the VertexMap of the MST by invoking grow V-1 times where V is the number of vertices.
        val (_, vertexMapResult) = vs.headOption match {
            case Some(v) =>
                val start: (Option[V], OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) = Some(v) -> OrderedVertexMap[V, UndirectedOrderedEdge[V, E]](v).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
                Range(0, vs.size).foldLeft(start) { (m, _) => grow(m) }
            case None => throw GraphException("doLazyPrim: empty vertex list")
        }

        vertexMapResult
    }
}