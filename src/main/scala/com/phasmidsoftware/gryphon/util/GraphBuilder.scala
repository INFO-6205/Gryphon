package com.phasmidsoftware.gryphon.util

import com.phasmidsoftware.gryphon.core._
import com.phasmidsoftware.parse.CellParser
import java.net.URL
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Utility class to help create graphs from edge lists, etc.
 */
class GraphBuilder[V: Ordering : CellParser, E: Ordering : CellParser, P: HasZero]() {

    type G = Graph[V, E, UndirectedOrderedEdge[V, E], P]

    def createFromUndirectedEdgeList(uy: Try[URL])(fv: String => Try[V], fe: String => Try[E]): Try[Iterable[UndirectedOrderedEdge[V, E]]] = {
        val eysy: Try[Iterator[Try[(V, V, E)]]] = for {
            u <- uy
            s = Source.fromURL(u)
        } yield for {
            string <- s.getLines()
            Array(wV1, wV2, wE) = string.split(" ")
        } yield for {
            v1 <- fv(wV1)
            v2 <- fv(wV2)
            e <- fe(wE)
        } yield (v1, v2, e)

        for {
            eys <- eysy
            es <- sequence(eys)
        } yield for {
            (v1, v2, e) <- es
            edge = UndirectedOrderedEdgeCase(v1, v2, e)
        } yield edge
    }

    def createGraphFromUndirectedOrderedEdges(esy: Try[Iterable[UndirectedOrderedEdge[V, E]]]): Try[G] =
        esy map {
            es =>
                // CONSIDER avoiding the two asInstanceOf calls
                val graph: G = UndirectedGraph[V, E, P]("no title").asInstanceOf[G]
                es.foldLeft(graph)((g, e) => g.addEdge(e))
        }

    private def sequence(eys: Iterator[Try[(V, V, E)]]): Try[List[(V, V, E)]] =
        eys.foldLeft(Try(List[(V, V, E)]())) { (xsy, ey) =>
            (xsy, ey) match {
                case (Success(xs), Success(e)) => Success(xs :+ e)
                case _ => Failure(GraphException("GraphBuilder: sequence error"))
            }
        }
}

object GraphBuilder {

    def resourceForClass(resourceName: String, clazz: Class[_] = getClass): Try[URL] = Option(clazz.getResource(resourceName)) match {
        case Some(u) => Success(u)
        case None => Failure(GraphException(s"$resourceName is not a valid resource for $clazz"))
    }

    def resource[C: ClassTag](resourceName: String): Try[URL] = resourceForClass(resourceName, implicitly[ClassTag[C]].runtimeClass)

}

object PrimDemo extends App {

    import GraphBuilder._

    private val resourceName = "/prim.graph"
    private val uy = resource(resourceName)
    private val gy = new GraphBuilder[Int, Double, Unit]().createFromUndirectedEdgeList(uy)(w => Try(w.toInt), w => Try(w.toDouble))
    gy match {
        case Success(g) =>
            println(s"read ${g.size} edges from $resourceName")
            println(g)
        case Failure(x) => throw x
    }
}