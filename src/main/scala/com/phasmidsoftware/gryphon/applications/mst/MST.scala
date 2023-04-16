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
        def processMinimumEdge(vertexMap: OrderedVertexMap[V, UndirectedOrderedEdge[V, E]], edge: UndirectedOrderedEdge[V, E]) = {
            val (v1, v2) = edge.vertices
            val (in, out) = if (vertexMap.contains(v1)) (v1, v2) else (v2, v1)
            Some(out) -> vertexMap.addEdge(in, edge).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
        }

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
        def grow(t: (Option[V], OrderedVertexMap[V, UndirectedOrderedEdge[V, E]])): (Option[V], OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) =
            t match {
                case (Some(v), m) =>
                    // CONSIDER putting some of this logic into VertexMap
                    val ws = vertexMapGraph.adjacentEdgesWithFilter(v)(x => !m.contains(x.other(v).get))
                    ws foreach (w => pq.addOne(w))
                    if (pq.isEmpty) None -> m
                    else processMinimumEdge(m, pq.dequeue())
                case (None, _) => t
            }

        // Starting at an arbitrary vertex of the graph (we pick the head of the vertices list),
        // gradually build up the VertexMap of the MST by invoking grow V-1 times where V is the number of vertices.
        val (_, vertexMapResult) = graph.vertices.headOption match {
            case Some(v) =>
                val start: (Option[V], OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) = Some(v) -> OrderedVertexMap[V, UndirectedOrderedEdge[V, E]](v).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
                Range(0, vertexMapGraph.size).foldLeft(start) { (m, _) => grow(m) }
            case None => throw GraphException("Prim.mst: empty graph")
        }

        // Return an undirected graph based on the generated vertexMap.
        UndirectedGraphCase[V, E](s"MST for graph ${graph.attribute}", vertexMapResult)
    }

}
