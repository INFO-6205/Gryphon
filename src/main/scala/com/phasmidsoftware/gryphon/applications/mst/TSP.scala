package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core._
import scala.util.Try

object TSP {

    def createEdgeFromVertices[V: Ordering, E: Ordering](v1: V, v2: V)(implicit d: (V, V) => E): UndirectedOrderedEdge[V, E] =
        UndirectedOrderedEdgeCase(v1, v2, d(v1, v2))

    def createEdgesFromVertices[V: Ordering, E: Ordering](vsy: Try[Iterable[V]])(implicit d: (V, V) => E): Try[Iterable[UndirectedOrderedEdge[V, E]]] = vsy map {
        vs =>
            for (v1 <- vs; v2 <- vs if Ordering[V].compare(v1, v2) < 0) yield createEdgeFromVertices(v1, v2)(implicitly[Ordering[V]], implicitly[Ordering[E]], d)
    }
}