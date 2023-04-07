package com.phasmidsoftware.gryphon.core

trait Graph[V, E, X <: Edge[V, E]] extends GraphLike[V, E] {

    def edges: Iterable[X]

    def vertices: Iterable[V] = adjacencies.keys

    def adjacencies: Map[V, AdjacencyList[X]]

    def addEdge(x: X): Graph[V, E, X]
}

abstract class AbstractGraph[V, E, X <: Edge[V, E]](val adjacencies: Map[V, AdjacencyList[X]]) extends Graph[V, E, X] {
    def allAdjacencies: AdjacencyList[X] = adjacencies.values.foldLeft(AdjacencyList.empty[X])(_ ++ _)

    def addXatV(adjacencyMap: Map[V, AdjacencyList[X]])(v: V, x: X): Map[V, AdjacencyList[X]] = adjacencyMap.-(v) + (v -> adjacencyMap(v).addEdge(x))

    def unit(adjacencyMap: Map[V, AdjacencyList[X]]): Graph[V, E, X]
}

abstract class AbstractDirectedGraph[V, E](val _adjacencyMap: Map[V, AdjacencyList[DirectedEdge[V, E]]]) extends AbstractGraph[V, E, DirectedEdge[V, E]](_adjacencyMap) {
    def edges: Iterable[DirectedEdge[V, E]] = allAdjacencies.xs

    def addEdge(x: DirectedEdge[V, E]): Graph[V, E, DirectedEdge[V, E]] = unit(addXatV(adjacencies)(x.from, x))
}

abstract class AbstractUndirectedGraph[V, E](val _adjacencyMap: Map[V, AdjacencyList[UndirectedEdge[V, E]]]) extends AbstractGraph[V, E, UndirectedEdge[V, E]](_adjacencyMap) {
    def edges: Iterable[UndirectedEdge[V, E]] = allAdjacencies.xs.distinct

    def addEdge(x: UndirectedEdge[V, E]): Graph[V, E, UndirectedEdge[V, E]] = {
        val (v, w) = x.vertices
        unit(addXatV(addXatV(adjacencies)(v, x))(w, x))
    }
}

trait GraphLike[V, E] {

}

case class DirectedGraph[V, E](_adjacencyList: Map[V, AdjacencyList[DirectedEdge[V, E]]]) extends AbstractDirectedGraph[V, E](_adjacencyList) {

    def unit(adjacencyMap: Map[V, AdjacencyList[DirectedEdge[V, E]]]): Graph[V, E, DirectedEdge[V, E]] = DirectedGraph(adjacencyMap)
}

object DirectedGraph {
    def apply[V, E]: DirectedGraph[V, E] = new DirectedGraph(Map.empty)
}

case class UndirectedGraph[V, E](_adjacencyList: Map[V, AdjacencyList[UndirectedEdge[V, E]]]) extends AbstractUndirectedGraph[V, E](_adjacencyList) {

    def unit(adjacencyMap: Map[V, AdjacencyList[UndirectedEdge[V, E]]]): Graph[V, E, UndirectedEdge[V, E]] = UndirectedGraph(adjacencyMap)
}

object UndirectedGraph {
    def apply[V, E]: UndirectedGraph[V, E] = new UndirectedGraph(Map.empty)
}