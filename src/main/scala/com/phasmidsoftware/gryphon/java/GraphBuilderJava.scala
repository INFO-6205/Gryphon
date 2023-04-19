package com.phasmidsoftware.gryphon.java

import com.phasmidsoftware.gryphon.core._
import com.phasmidsoftware.gryphon.util.Util.tryNonNull
import com.phasmidsoftware.util.FP.resource
import java.util.{Optional, function}
import scala.compat.java8.OptionConverters.RichOptionForJava8
import scala.jdk.CollectionConverters._
import scala.util.Try


/**
 * This GraphBuilder is intended to be called from Java.
 */
case class GraphBuilderJava[V: Ordering, E: Ordering, P: HasZero](gb: com.phasmidsoftware.gryphon.util.GraphBuilder[V, E, Unit]) {

    def createUndirectedEdgeList(u: String): Optional[java.util.List[UndirectedOrderedEdge[V, E]]] = {
        gb.createUndirectedEdgeList(resource(u)).toOption.map(_.toSeq.asJava).asJava
    }

    def createGraphFromUndirectedOrderedEdges(esy: Try[Iterable[UndirectedOrderedEdge[V, E]]]): Try[Graph[V, E, UndirectedOrderedEdge[V, E], Unit]] = gb.createGraphFromUndirectedOrderedEdges(esy)
}

object GraphBuilderJava {
    def create[V <: Comparable[V], E <: Comparable[E]](fV: java.util.function.Function[String, V], fE: java.util.function.Function[String, E]): GraphBuilderJava[V, E, Unit] = {
        implicit object ParseableV extends Parseable[V] {
            def parse(w: String): Try[V] = parseString(fV, "V")(w)
        }
        implicit object ParseableE extends Parseable[E] {
            def parse(w: String): Try[E] = parseString(fE, "E")(w)
        }
        GraphBuilderJava(new com.phasmidsoftware.gryphon.util.GraphBuilder[V, E, Unit])
    }

    private def parseString[T](f: function.Function[String, T], genericType: String)(w: String): Try[T] =
        tryNonNull(f(w), GraphException(s"Java GraphBuilder.apply: cannot parse $w as a " + genericType))
}