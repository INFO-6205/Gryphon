package com.phasmidsoftware.gryphon.applications.mst

import com.phasmidsoftware.gryphon.core._
import java.net.URL
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object GraphBuilder {

    def createFromUndirectedEdgeList[V: Ordering, E: Ordering](uy: Try[URL])(fv: String => Try[V], fe: String => Try[E]): Try[Graph[V, E, UndirectedEdge[V, E]]] = {
        val eysy: Try[Iterator[Try[(V, V, E)]]] = for {
            u <- uy
            s = Source.fromURL(u)
        } yield for {
            string <- s.getLines()
            _ = println(string)
            Array(wV1, wV2, wE) = string.split(" ")
        } yield for {
            v1 <- fv(wV1)
            v2 <- fv(wV2)
            e <- fe(wE)
        } yield (v1, v2, e)

        eysy map (eys => println(eys.toSeq))

        val esy: Try[List[UndirectedEdge[V, E]]] = for {
            eys <- eysy
            es <- sequence(eys)
        } yield for {
            (v1, v2, e) <- es
            edge = UndirectedEdgeCase(v1, v2, e)
        } yield edge

        esy map {
            val graph: Graph[V, E, UndirectedEdge[V, E]] = UndirectedGraph[V, E]("no title")
            es => es.foldLeft(graph)((g, e) => g.addEdge(e))
        }
    }

    private def sequence[V, E](eys: Iterator[Try[(V, V, E)]]): Try[List[(V, V, E)]] =
        eys.foldLeft(Try(List[(V, V, E)]())) { (xsy, ey) =>
            (xsy, ey) match {
                case (Success(xs), Success(e)) => Success(xs :+ e)
                case _ => Failure(GraphException("GraphBuilder: sequence error"))
            }
        }

    def resourceForClass(resourceName: String, clazz: Class[_] = getClass): Try[URL] = Option(clazz.getResource(resourceName)) match {
        case Some(u) => Success(u)
        case None => Failure(GraphException(s"$resourceName is not a valid resource for $clazz"))
    }

    def resource[C: ClassTag](resourceName: String): Try[URL] = resourceForClass(resourceName, implicitly[ClassTag[C]].runtimeClass)

}

object PrimDemo extends App {

    import GraphBuilder._

    private val uy = resource("/prim.graph")
    private val gy = createFromUndirectedEdgeList[Int, Double](uy)(w => Try(w.toInt), w => Try(w.toDouble))
    gy match {
        case Success(g) => println(g)
        case Failure(x) => throw x
    }
}