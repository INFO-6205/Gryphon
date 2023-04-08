package com.phasmidsoftware.gryphon.core

/**
 * Class which represents an adjacency list.
 *
 * @param xs a sequence of edges of type X.
 * @tparam X the type of edge.
 */
case class AdjacencyList[+X](xs: Seq[X]) {

    /**
     * Method to concatenate this with another AdjacencyList.
     *
     * @param a the other AdjacencyList.
     * @tparam Y the underlying type of <code>a</code> and of the result.
     * @return an AdjacencyList[Y].
     */
    def ++[Y >: X](a: AdjacencyList[Y]): AdjacencyList[Y] = AdjacencyList(xs ++ a.xs)

    /**
     * Method to yield the size of this AdjacencyList.
     *
     * @return the number of X elements in this AdjacencyList.
     */
    def size: Int = xs.size
}

object AdjacencyList {
    /**
     * Method to create an empty AdjacencyList.
     *
     * @tparam X the underlying type of the result.
     * @return an AdjacencyList[X].
     */
    def empty[X]: AdjacencyList[X] = AdjacencyList(Nil)
}
