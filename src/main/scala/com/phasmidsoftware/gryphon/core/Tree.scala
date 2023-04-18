package com.phasmidsoftware.gryphon.core

/**
 * Trait to model the behavior of a Tree.
 *
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 */
trait Tree[V, E, X <: UndirectedEdge[V, E]] extends UndirectedGraph[V, E, X] {
    override def isCyclic: Boolean = false // TODO we should be able to assert this

    override def isBipartite: Boolean = true
}

/**
 * Case class to represent a tree.
 *
 * @param description the description of this tree.
 * @param vertexMap   the vertex map to define this tree.
 * @tparam V the (key) vertex-attribute type.
 * @tparam E the edge-attribute type.
 *
 */
case class TreeCase[V, E, X <: UndirectedEdge[V, E]](description: String, vertexMap: VertexMap[V, X]) extends AbstractUndirectedGraph[V, E, X](description, vertexMap) with Tree[V, E, X] {

    /**
     * Method to create a new AbstractGraph from a given vertex map.
     *
     * CONSIDER add an attribute parameter.
     *
     * @param vertexMap the vertex map.
     * @return a new AbstractGraph[V, E].
     */
    def unit(vertexMap: VertexMap[V, X]): AbstractGraph[V, E, X] = TreeCase[V, E, X]("no description", vertexMap)

}