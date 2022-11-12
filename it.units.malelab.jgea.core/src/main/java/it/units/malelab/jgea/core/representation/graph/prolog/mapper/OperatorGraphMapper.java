package it.units.malelab.jgea.core.representation.graph.prolog.mapper;

import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.Node;
import it.units.malelab.jgea.core.representation.graph.numeric.Constant;
import it.units.malelab.jgea.core.representation.graph.numeric.Output;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.BaseOperator;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.OperatorGraph;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.OperatorNode;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.analysis.TreeAnalysis;

import java.util.*;

public class OperatorGraphMapper { //implements Function<PrologGraph, OperatorGraph>

  public static OperatorGraph apply(PrologGraph prologTree) {
    LinkedHashGraph<Node, OperatorGraph.NonValuedArc> intermediateGraph = new LinkedHashGraph<>();
    int index = 1; //0 reserved for output node
    LinkedHashMap<String, Node> idToNode = new LinkedHashMap<>();
    final BaseOperator[] baseOperators = new BaseOperator[]{BaseOperator.ADDITION, BaseOperator.DIVISION, BaseOperator.MULTIPLICATION, BaseOperator.SUBTRACTION};
    final String[] baseOperatorsString = new String[baseOperators.length];
    for (int i = 0; i < baseOperators.length; ++i) {
      baseOperatorsString[i] = baseOperators[i].toString();
    }

    Output outputNode = new Output(0);
    intermediateGraph.addNode(outputNode);
    for (Map<String, Object> node : prologTree.nodes()) {
      Node tmpNode;

      if (node.get("type").toString().equalsIgnoreCase("operator")) {
        String prologOperator = node.get("value").toString().replace("'", "");
        final int indexOfOperator = Arrays.asList(baseOperatorsString).indexOf(prologOperator);
        tmpNode = new OperatorNode(index, baseOperators[indexOfOperator]);

        if (node.get("start").toString().equals("1")) {
          idToNode.put(node.get("node_id").toString(), tmpNode);
          intermediateGraph.addNode(tmpNode);
          index++;
          intermediateGraph.setArcValue(tmpNode, outputNode, OperatorGraph.NON_VALUED_ARC);
          continue;
        }
      } else if (node.get("type").toString().equals("variable")) {
        String valueString = node.get("value").toString();
        double value = Double.parseDouble(valueString);
        tmpNode = new Constant(index, value);
      } else {
        throw new UnsupportedOperationException("Not acceptable type");
      }

      idToNode.put(node.get("node_id").toString(), tmpNode);
      intermediateGraph.addNode(tmpNode);
      index++;
    }

    for (Graph.Arc<Map<String, Object>> arc : prologTree.arcs()) {
      intermediateGraph.setArcValue(idToNode.get((String) arc.getSource().get("node_id")), idToNode.get((String) arc.getTarget().get("node_id")), OperatorGraph.NON_VALUED_ARC);
    }
    return new OperatorGraph(intermediateGraph);
  }


  public static void main(String[] args) {

    //// Domain
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");

    List<String> operatorValues = Arrays.asList("+", "/", "*", "-");

    // Generate graph
    PrologGraph binaryTree = TreeAnalysis.generateBinaryTreeGraph(10, domainDefinition, operatorValues);
    System.out.println("\nProlog graph.\nArcs: " + binaryTree.arcs());

    OperatorGraph convertedGraph = apply(binaryTree);
    System.out.println(convertedGraph);


  }

}


