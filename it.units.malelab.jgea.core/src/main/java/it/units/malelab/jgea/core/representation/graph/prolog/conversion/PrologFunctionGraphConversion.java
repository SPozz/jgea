package it.units.malelab.jgea.core.representation.graph.prolog.conversion;

import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.Node;
import it.units.malelab.jgea.core.representation.graph.numeric.Input;
import it.units.malelab.jgea.core.representation.graph.numeric.Output;
import it.units.malelab.jgea.core.representation.graph.numeric.functiongraph.BaseFunction;
import it.units.malelab.jgea.core.representation.graph.numeric.functiongraph.FunctionGraph;
import it.units.malelab.jgea.core.representation.graph.numeric.functiongraph.FunctionNode;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.analysis.FfnnAnalysis;

import java.util.*;

public class PrologFunctionGraphConversion {

  public static FunctionGraph convert(PrologGraph prologFfnn) {
    LinkedHashGraph<Node, Double> intermediateGraph = new LinkedHashGraph<>();
    int index = 0;
    LinkedHashMap<String, Node> idToNode = new LinkedHashMap<>();

    Set<Integer> levels = new HashSet<>();
    for (Map<String, Object> node : prologFfnn.nodes()) {
      String nodeLevel = node.get("layer").toString();
      levels.add(Integer.parseInt(nodeLevel));
    }
    int maxLevel = Collections.max(levels);
    int minLevel = Collections.min(levels);

    for (Map<String, Object> node : prologFfnn.nodes()) {
      Node tmpNode;

      int nodeLevel = Integer.parseInt(node.get("layer").toString());

      if (nodeLevel == minLevel) {
        tmpNode = new Input(index);
      } else if (nodeLevel == maxLevel) {
        tmpNode = new Output(index);
      } else {
        tmpNode = new FunctionNode(index, BaseFunction.IDENTITY); //TODO: change according to actual function
      }

      idToNode.put(node.get("node_id").toString(), tmpNode);
      intermediateGraph.addNode(tmpNode);
      index++;
    }

    for (Graph.Arc<Map<String, Object>> arc : prologFfnn.arcs()) {
      Node source = idToNode.get((String) arc.getSource().get("node_id"));
      Node target = idToNode.get((String) arc.getTarget().get("node_id"));
      Double weight = Double.parseDouble(prologFfnn.getArcValue(arc).get("weight").toString());
      intermediateGraph.setArcValue(source, target, weight);
    }

    return new FunctionGraph(intermediateGraph);
  }


  public static void main(String[] args) {
    //// Domain
    List<String> domainDefinition = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic layer/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic weight/2."
    );


    // Generate graph
    PrologGraph ffnn = FfnnAnalysis.generateFfnnGraph(50, domainDefinition);

    FunctionGraph resultingGraph = convert(ffnn);
    System.out.println(resultingGraph);

  }
}
