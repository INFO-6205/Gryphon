package com.phasmidsoftware.gryphon.core

trait Graph[V, E, X <: Edge[V, E]] extends GraphLike[V, E] {

    def edges: Iterable[X]

    def vertices: Iterable[V] = vertexMap.keys

    def vertexMap: Map[V, Vertex[V, X]]

    def addEdge(x: X): Graph[V, E, X]
}

abstract class AbstractGraph[V, E, X <: Edge[V, E]](val __vertexMap: Map[V, Vertex[V, X]]) extends Graph[V, E, X] {
    def allAdjacencies: AdjacencyList[X] = __vertexMap.values.foldLeft(AdjacencyList.empty[X])(_ ++ _.adjacent)

    def addXatV(vertexMap: Map[V, Vertex[V, X]])(v: V, x: X): Map[V, Vertex[V, X]] = vertexMap.-(v) + (v -> vertexMap(v).addEdge(x))

    def unit(vertexMap: Map[V, Vertex[V, X]]): Graph[V, E, X]
}

abstract class AbstractDirectedGraph[V, E](val _vertexMap: Map[V, Vertex[V, BaseDirectedEdge[V, E]]]) extends AbstractGraph[V, E, BaseDirectedEdge[V, E]](_vertexMap) {
    def edges: Iterable[BaseDirectedEdge[V, E]] = allAdjacencies.xs

    def addEdge(x: BaseDirectedEdge[V, E]): Graph[V, E, BaseDirectedEdge[V, E]] = unit(addXatV(vertexMap)(x.from, x))
}

abstract class AbstractUndirectedGraph[V, E](val _vertexMap: Map[V, Vertex[V, BaseUndirectedEdge[V, E]]]) extends AbstractGraph[V, E, BaseUndirectedEdge[V, E]](_vertexMap) {
    def edges: Iterable[BaseUndirectedEdge[V, E]] = allAdjacencies.xs.distinct

    def addEdge(x: BaseUndirectedEdge[V, E]): Graph[V, E, BaseUndirectedEdge[V, E]] = {
        val (v, w) = x.vertices
        unit(addXatV(addXatV(vertexMap)(v, x))(w, x))
    }
}

trait GraphLike[V, E] {

}

case class DirectedGraph[V, E](vertexMap: Map[V, Vertex[V, BaseDirectedEdge[V, E]]]) extends AbstractDirectedGraph[V, E](vertexMap) {

    def unit(vertexMap: Map[V, Vertex[V, BaseDirectedEdge[V, E]]]): Graph[V, E, BaseDirectedEdge[V, E]] = DirectedGraph(vertexMap)
}

object DirectedGraph {
    def apply[V, E]: DirectedGraph[V, E] = new DirectedGraph(Map.empty)
}

case class UndirectedGraph[V, E](vertexMap: Map[V, Vertex[V, BaseUndirectedEdge[V, E]]]) extends AbstractUndirectedGraph[V, E](vertexMap) {

    def unit(vertexMap: Map[V, Vertex[V, BaseUndirectedEdge[V, E]]]): Graph[V, E, BaseUndirectedEdge[V, E]] = UndirectedGraph(vertexMap)
}

object UndirectedGraph {
    def apply[V, E]: UndirectedGraph[V, E] = new UndirectedGraph(Map.empty)
}