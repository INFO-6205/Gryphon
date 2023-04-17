package com.phasmidsoftware.gryphon.core

trait DAG[V, E, X <: DirectedEdge[V, E]] extends DirectedGraph[V, E, X] {
    override def isCyclic: Boolean = false // TODO we should be able to assert this

    override def isBipartite: Boolean = false
}
