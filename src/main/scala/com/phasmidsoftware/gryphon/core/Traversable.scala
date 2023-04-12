package com.phasmidsoftware.gryphon.core

/**
 * Trait to define the behavior of a graph-like structure which can be traversed by dfs, bfs, etc.
 *
 * @tparam V the underlying key (attribute) type for a vertex.
 */
trait Traversable[V] {

    /**
     * Method to run depth-first-search on this Traversable.
     *
     * @param visitor the visitor, of type Visitor[V, J].
     * @param v       the starting vertex.
     * @tparam J the journal type.
     * @return a new Visitor[V, J].
     */
    def dfs[J](visitor: Visitor[V, J])(v: V): Visitor[V, J]

    /**
     * Method to run breadth-first-search on this Traversable.
     *
     * @param visitor the visitor, of type Visitor[V, J].
     * @param v       the starting vertex.
     * @tparam J the journal type.
     * @return a new Visitor[V, J].
     */
    def bfs[J](visitor: Visitor[V, J])(v: V): Visitor[V, J]

    /**
     * Method to run breadth-first-search with a mutable queue on this Traversable.
     *
     * @param visitor the visitor, of type Visitor[V, J].
     * @param v       the starting vertex.
     * @tparam J the journal type.
     * @return a new Visitor[V, J].
     */
    def bfsMutable[J](visitor: Visitor[V, J])(v: V): Visitor[V, J]
}
