package com.phasmidsoftware.gryphon.core

trait Graph[V, X <: Edge[V, E], E] extends GraphLike[V, E] {

    def edges: Iterable[X]

    def vertices: Iterable[V] = adjacencyMap.keys

    def adjacencyMap: Map[V, AdjacencyList[X]]

    def addEdge(x: X): Graph[V, X, E]
}

abstract class AbstractGraph[V, X <: Edge[V, E], E](val adjacencyMap: Map[V, AdjacencyList[X]]) extends Graph[V, X, E] {
    def allAdjacencies: AdjacencyList[X] = adjacencyMap.values.foldLeft(AdjacencyList.empty[X])(_ ++ _)

    def addXatV(adjacencyMap: Map[V, AdjacencyList[X]])(v: V, x: X): Map[V, AdjacencyList[X]] = adjacencyMap.-(v) + (v -> adjacencyMap(v).addEdge(x))

    def unit(adjacencyMap: Map[V, AdjacencyList[X]]): Graph[V, X, E]
}

abstract class AbstractDirectedGraph[V, E](val _adjacencyMap: Map[V, AdjacencyList[DirectedEdge[V, E]]]) extends AbstractGraph[V, DirectedEdge[V, E], E](_adjacencyMap) {
    def edges: Iterable[DirectedEdge[V, E]] = allAdjacencies.xs

    def addEdge(x: DirectedEdge[V, E]): Graph[V, DirectedEdge[V, E], E] = unit(addXatV(adjacencyMap)(x.from, x))
}

abstract class AbstractUndirectedGraph[V, E](val _adjacencyMap: Map[V, AdjacencyList[UndirectedEdge[V, E]]]) extends AbstractGraph[V, UndirectedEdge[V, E], E](_adjacencyMap) {
    def edges: Iterable[UndirectedEdge[V, E]] = allAdjacencies.xs.distinct

    def addEdge(x: UndirectedEdge[V, E]): Graph[V, UndirectedEdge[V, E], E] = {
        val (v, w) = x.vertices
        unit(addXatV(addXatV(adjacencyMap)(v, x))(w, x))
    }
}

trait GraphLike[V, E] {

}

case class DirectedGraph[V, E](_adjacencyList: Map[V, AdjacencyList[DirectedEdge[V, E]]]) extends AbstractDirectedGraph[V, E](_adjacencyList) {

    def unit(adjacencyMap: Map[V, AdjacencyList[DirectedEdge[V, E]]]): Graph[V, DirectedEdge[V, E], E] = DirectedGraph(adjacencyMap)
}

object DirectedGraph {
    def apply[V, E]: DirectedGraph[V, E] = new DirectedGraph(Map.empty)
}

case class UndirectedGraph[V, E](_adjacencyList: Map[V, AdjacencyList[UndirectedEdge[V, E]]]) extends AbstractUndirectedGraph[V, E](_adjacencyList) {

    def unit(adjacencyMap: Map[V, AdjacencyList[UndirectedEdge[V, E]]]): Graph[V, UndirectedEdge[V, E], E] = UndirectedGraph(adjacencyMap)
}

object UndirectedGraph {
    def apply[V, E]: UndirectedGraph[V, E] = new UndirectedGraph(Map.empty)
}