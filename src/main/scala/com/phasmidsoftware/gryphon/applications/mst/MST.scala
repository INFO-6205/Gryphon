package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core._
import scala.collection.mutable

trait MST[V, E] {

    /**
     * Abstract method to calculate the Minimum Spanning Tree of a graph.
     *
     * @param graph the graph whose MST is required.
     * @return the MST for graph.
     */
    def mst(graph: UndirectedGraph[V, E, UndirectedOrderedEdge[V, E]]): UndirectedGraph[V, E, UndirectedEdge[V, E]]
}

abstract class BaseMST[V: Ordering, E: Ordering] extends MST[V, E] {

}

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

            def compare(x: UndirectedOrderedEdge[V, E], y: UndirectedOrderedEdge[V, E]): Int = implicitly[Ordering[E]].compare(x.attribute, y.attribute)
        }

        val pq = new mutable.PriorityQueue[UndirectedOrderedEdge[V, E]]

        def otherEndInTree(m: OrderedVertexMap[V, UndirectedOrderedEdge[V, E]], x: UndirectedOrderedEdge[V, E], v: V) =
            m.contains(x.other(v).get)

        def grow(t: (V, OrderedVertexMap[V, UndirectedOrderedEdge[V, E]])): (V, OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) = {
            // CONSIDER putting some of this logic into VertexMap
            val (v, m) = t
            val xs: Seq[UndirectedOrderedEdge[V, E]] = vertexMapGraph.adjacentEdges(v)
            val ws: Seq[UndirectedOrderedEdge[V, E]] = xs filterNot (x => otherEndInTree(m, x, v))
            ws foreach (w => pq.addOne(w))
            val x = pq.dequeue()
            val (v1, v2) = x.vertices
            val (in, out) = if (m.contains(v1)) (v1, v2) else (v2, v1)
            out -> m.addEdge(in, x).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
        }

        val V = vertexMapGraph.size
        val (_, vertexMapResult) = graph.vertices.headOption match {
            case Some(v) =>
                val start: (V, OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]) = v -> OrderedVertexMap[V, UndirectedOrderedEdge[V, E]](v).asInstanceOf[OrderedVertexMap[V, UndirectedOrderedEdge[V, E]]]
                Range(0, V - 1).foldLeft(start) { (m, _) => grow(m) }
            case None => throw GraphException("Prim.mst: empty graph")
        }
        UndirectedGraphCase[V, E](s"MST for graph ${graph.attribute}", vertexMapResult)

    }
}