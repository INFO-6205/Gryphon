package com.phasmidsoftware.gryphon.java;

import com.phasmidsoftware.gryphon.core.UndirectedOrderedEdge;
import scala.runtime.BoxedUnit;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;

public class GraphBuilder<V extends Comparable<V>, E extends Comparable<E>> {
    public GraphBuilder(Function<String, V> vParser, Function<String, E> eParser) {
        gb = com.phasmidsoftware.gryphon.java.GraphBuilderJava.create(vParser, eParser);
    }

    public Optional<List<UndirectedOrderedEdge<V, E>>>
    createUndirectedEdgeList(String resource) {
        return gb.createUndirectedEdgeList(resource);
    }

    private final com.phasmidsoftware.gryphon.java.GraphBuilderJava<V, E, BoxedUnit> gb;
}
