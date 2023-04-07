package com.phasmidsoftware.gryphon.core

trait Vertex[V, X] extends VertexLike[V] {
    def addEdge(x: X): Vertex[V, X]

    def adjacent: AdjacencyList[X]


    def degree: Int = adjacent.size
}

abstract class AbstractVertex[V, X] extends Vertex[V, X] {
    def addEdge(x: X): Vertex[V, X] = unit(AdjacencyList(x +: adjacent.xs))

    def unit[Y](adjacent: AdjacencyList[Y]): AbstractVertex[V, Y]
}

trait VertexLike[V] {
    def attribute: V
}
