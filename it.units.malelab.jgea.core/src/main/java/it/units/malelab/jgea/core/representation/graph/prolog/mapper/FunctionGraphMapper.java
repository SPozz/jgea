package it.units.malelab.jgea.core.representation.graph.prolog.mapper;

import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.Node;
import it.units.malelab.jgea.core.representation.graph.numeric.Input;
import it.units.malelab.jgea.core.representation.graph.numeric.Output;
import it.units.malelab.jgea.core.representation.graph.numeric.functiongraph.BaseFunction;
import it.units.malelab.jgea.core.representation.graph.numeric.functiongraph.FunctionGraph;
import it.units.malelab.jgea.core.representation.graph.numeric.functiongraph.FunctionNode;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;

import java.util.*;
import java.util.function.Function;

public class FunctionGraphMapper implements Function<PrologGraph, FunctionGraph> {
  private final BaseFunction function;

  public FunctionGraphMapper(BaseFunction function) {
    this.function = function;
  }

  public FunctionGraph apply(PrologGraph prologFfnn) {
    LinkedHashGraph<Node, Double> intermediateGraph = new LinkedHashGraph<>();
    int inputIndex = 0;
    int functionIndex = 0;
    int outputIndex = 0;
    LinkedHashMap<String, Node> idToNode = new LinkedHashMap<>();
    Set<Integer> levels = new HashSet<>();
    for (Map<String, Object> node : prologFfnn.nodes()) {
      String nodeLevel = node.get("layer").toString();
      levels.add(Integer.parseInt(nodeLevel));
    }
    final int maxLevel = Collections.max(levels);
    final int minLevel = Collections.min(levels);

    for (Map<String, Object> node : prologFfnn.nodes()) {
      Node tmpNode;
      final int nodeLevel = Integer.parseInt(node.get("layer").toString());
      if (nodeLevel == minLevel) {
        tmpNode = new Input(inputIndex, inputIndex);
        inputIndex++;
      } else if (nodeLevel == maxLevel) {
        tmpNode = new Output(outputIndex);
        outputIndex++;
      } else {
        tmpNode = new FunctionNode(functionIndex, function);
        functionIndex++;
      }
      idToNode.put(node.get("node_id").toString(), tmpNode);
      intermediateGraph.addNode(tmpNode);
    }
    for (Graph.Arc<Map<String, Object>> arc : prologFfnn.arcs()) {
      Node source = idToNode.get((String) arc.getSource().get("node_id"));
      Node target = idToNode.get((String) arc.getTarget().get("node_id"));
      Double weight = Double.parseDouble(prologFfnn.getArcValue(arc).get("weight").toString());
      intermediateGraph.setArcValue(source, target, weight);
    }
    return new FunctionGraph(intermediateGraph);
  }
}
