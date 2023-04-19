package com.phasmidsoftware.gryphon.java;

import com.phasmidsoftware.gryphon.core.Graph;
import com.phasmidsoftware.gryphon.core.UndirectedOrderedEdge;
import org.junit.Test;
import scala.runtime.BoxedUnit;

import java.util.List;
import java.util.Optional;

public class GraphBuilderTest {

    @Test
    public void createUndirectedEdgeList() {
        GraphBuilderJava<String, String, BoxedUnit> gb = GraphBuilderJava.create(w -> w, w -> w);
        Optional<List<UndirectedOrderedEdge<String, String>>> maybeEdges = gb.createUndirectedEdgeList("/prim.graph");
        System.out.println(maybeEdges);
        Optional<Graph<String, String, UndirectedOrderedEdge<String, String>, BoxedUnit>> maybeGraph = gb.createGraphFromUndirectedEdgeList(maybeEdges);
        System.out.println(maybeGraph);
    }
}