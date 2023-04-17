package com.phasmidsoftware.gryphon.core

trait Tree[V, E] extends UndirectedGraph[V, E, UndirectedEdge[V, E]] {
    override def isCyclic: Boolean = false // TODO we should be able to assert this

    override def isBipartite: Boolean = true
}

case class TreeCase[V, E](description: String, vertexMap: VertexMap[V, UndirectedEdge[V, E]]) extends AbstractUndirectedGraph[V, E](description, vertexMap) with Tree[V, E] {

    /**
     * Method to create a new AbstractGraph from a given vertex map.
     *
     * CONSIDER add an attribute parameter.
     *
     * @param vertexMap the vertex map.
     * @return a new AbstractGraph[V, E].
     */
    def unit(vertexMap: VertexMap[V, UndirectedEdge[V, E]]): AbstractGraph[V, E, UndirectedEdge[V, E]] = TreeCase[V, E]("no description", vertexMap)

}