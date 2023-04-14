package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core._
import scala.collection.mutable

/**
 * Trait to model the behavior of a minimum spanning tree.
 * This works only for undirected graphs.
 *
 * @tparam V the vertex (key) attribute type.
 * @tparam E the edge type.
 */
trait MST[V, E] {

    /**
     * Abstract method to calculate the Minimum Spanning Tree of a graph.
     *
     * @param graph the graph whose MST is required.
     * @return the MST for graph.
     */
    def mst(graph: UndirectedGraph[V, E, UndirectedOrderedEdge[V, E]]): UndirectedGraph[V, E, UndirectedEdge[V, E]]
}

/**
 * Abstract class to implement MST[V, E].
 *
 * @tparam V the vertex (key) attribute type.
 *           Requires implicit evidence of type Ordering[V].
 * @tparam E the edge type.
 *           Requires implicit evidence of type Ordering[E].
 */
abstract class BaseMST[V: Ordering, E: Ordering] extends MST[V, E]

/**
 * Implementation class which evaluates the MST using Prim's algorithm.
 *
 * @tparam V the vertex (key) attribute type.
 *           Requires implicit evidence of type Ordering[V].
 * @tparam E the edge type.
 *           Requires implicit evidence of type Ordering[E].
 */
class Prim[V: Ordering, E: Ordering] extends BaseMST[V, E] {

    /**
     * Abstract method to calculate the Minimum Spanning Tree of a graph.
     *
     * @param graph the graph whose MST is required.
     * @return the MST for graph.
     */
    def mst(graph: UndirectedGraph[V, E, UndirectedOrderedEdge[V, E]]): UndirectedGraph[V, E, UndirectedEdge[V, E]] = {
        val vertexMapGraph: VertexMap[V, UndirectedOrderedEdge[V, E]] = graph.vertexMap
        implicit object UndirectedEdgeOrdering extends Ordering[UndirectedOrderedEdge[V, E]] {

            def compare(x: UndirectedOrderedEdge[V, E], y: UndirectedOrderedEdge[V, E]): Int = OrderedEdge.compare(x, y)
        }
        val pq = new mutable.PriorityQueue[UndirectedOrderedEdge[V, E]]


        /**
         * Method to grow the tree according to Prim's algorithm.
         * CONSIDER using bfs for this.
         *
         * @param t a tuple consisting of (the vertex most recently added to the tree, the current VertexMap).
         * @return a tuple of (most recently added vertex, new vertex map).
         */
        def grow(t: (V, OrderedVertexMap[V, UndirectedOrderedEdge[V, E]])): (V, OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) = {
            // CONSIDER putting some of this logic into VertexMap
            val (v, m) = t
            val ws = vertexMapGraph.adjacentEdgesWithFilter(v)(x => !m.contains(x.other(v).get))
            ws foreach (w => pq.addOne(w))
            val x = pq.dequeue()
            val (v1, v2) = x.vertices
            val (in, out) = if (m.contains(v1)) (v1, v2) else (v2, v1)
            out -> m.addEdge(in, x).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
        }

        // Starting at an arbitrary vertex of the graph (we pick the head of the vertices list),
        // gradually build up the VertexMap of the MST by invoking grow V-1 times where V is the number of vertices.
        val (_, vertexMapResult) = graph.vertices.headOption match {
            case Some(v) =>
                val start: (V, OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) = v -> OrderedVertexMap[V, UndirectedOrderedEdge[V, E]](v).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
                Range(0, vertexMapGraph.size - 1).foldLeft(start) { (m, _) => grow(m) }
            case None => throw GraphException("Prim.mst: empty graph")
        }

        // Return an undirected graph based on the generated vertexMap.
        UndirectedGraphCase[V, E](s"MST for graph ${graph.attribute}", vertexMapResult)
    }

}
