package com.phasmidsoftware.gryphon.core

case class AdjacencyList[+X](xs: Seq[X]) {
    def addEdge[Y >: X](y: Y): AdjacencyList[Y] = AdjacencyList(y +: xs)

    def ++[Y >: X](a: AdjacencyList[Y]): AdjacencyList[Y] = AdjacencyList(xs ++ a.xs)

    def size: Int = xs.size
}

object AdjacencyList {
    def empty[X]: AdjacencyList[X] = AdjacencyList(Nil)
}
