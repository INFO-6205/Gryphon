package com.phasmidsoftware.gryphon.core

trait Vertex[V, E] extends VertexLike[V] {
    def adjacent: Iterable[E]

    def degree: Int = adjacent.size
}

trait VertexLike[V] {
    def attribute: V
}
