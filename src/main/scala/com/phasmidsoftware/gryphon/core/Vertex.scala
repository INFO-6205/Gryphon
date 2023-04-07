package com.phasmidsoftware.gryphon.core

trait Vertex[V, X] extends VertexLike[V] {
    def adjacent: AdjacencyList[X]

    def degree: Int = adjacent.size
}

trait VertexLike[V] {
    def attribute: V
}
