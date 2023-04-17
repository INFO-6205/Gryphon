package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core._
import com.phasmidsoftware.gryphon.util.LazyPriorityQueue
import scala.util.Try

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
 * Implementation class which evaluates the MST using Prim's (lazy) algorithm.
 *
 * @tparam V the vertex (key) attribute type.
 *           Requires implicit evidence of type Ordering[V].
 * @tparam E the edge type.
 *           Requires implicit evidence of type Ordering[E].
 */
class LazyPrim[V: Ordering, E: Ordering] extends BaseMST[V, E] {

    /**
     * Abstract method to calculate the Minimum Spanning Tree of a graph.
     *
     * @param graph the graph whose MST is required.
     * @return the MST for graph.
     */
    def mst(graph: UndirectedGraph[V, E, UndirectedOrderedEdge[V, E]]): UndirectedGraph[V, E, UndirectedEdge[V, E]] = {

        def hasExactlyOneVertexInMap(m: OrderedVertexMap[V, UndirectedOrderedEdge[V, E]])(e: UndirectedOrderedEdge[V, E]) =
            !m.contains(e.vertices._1) ^ !m.contains(e.vertices._2)

        val vertexMapGraph: VertexMap[V, UndirectedOrderedEdge[V, E]] = graph.vertexMap
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
                    val ws = vertexMapGraph.adjacentEdgesWithFilter(v)(x => !m.contains(x.other(v).get))
                    ws foreach (w => pq.addOne(w))
                    // NOTE: because this is the lazy version of Prim, we must still check that other vertex of e is not in the vertex map.
                    pq.conditionalDequeue(hasExactlyOneVertexInMap(m)) match {
                        case Some(e) => m.addEdgeWithVertex(e)
                        case None => None -> m
                    }
                case (None, _) => t
            }


        // Starting at an arbitrary vertex of the graph (we pick the head of the vertices list),
        // gradually build up the VertexMap of the MST by invoking grow V-1 times where V is the number of vertices.
        val (_, vertexMapResult) = graph.vertices.headOption match {
            case Some(v) =>
                val start: (Option[V], OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) = Some(v) -> OrderedVertexMap[V, UndirectedOrderedEdge[V, E]](v).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
                Range(0, vertexMapGraph.size).foldLeft(start) { (m, _) => grow(m) }
            case None => throw GraphException("LazyPrim.mst: empty graph")
        }

        // Return an undirected graph based on the generated vertexMap.
        UndirectedGraphCase[V, E](s"MST for graph ${graph.attribute}", vertexMapResult)
    }
}

trait EdgeData[V, E] {
    def vertex1: V

    def vertex2: V

    def edge: E
}

case class EdgeDataMST[V, E](vertex1: V, vertex2: V, edge: E) extends EdgeData[V, E]
