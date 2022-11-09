package it.units.malelab.jgea.core.representation.graph.prolog;


import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.Node;
import it.units.malelab.jgea.core.representation.graph.numeric.Input;
import it.units.malelab.jgea.core.representation.graph.numeric.Output;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.BaseOperator;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.OperatorGraph;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.OperatorNode;
import it.units.malelab.jgea.core.representation.graph.prolog.analysis.TreeAnalysis;

import java.util.*;

public class PrologOperatorGraphConversion {

  public static void convert(PrologGraph prologTree) { // return type should be OperatorGraph
    // convert Prolog Tree into Graph<Node, NonValuedArc>

    LinkedHashGraph<Node, OperatorGraph.NonValuedArc> intermediateGraph = new LinkedHashGraph<>();
    int index = 0;

    LinkedHashMap<String, Integer> nodeConversionDictionary = new LinkedHashMap<>();

    LinkedHashMap<Integer, Node> indexToNode = new LinkedHashMap<>();

    for (Map<String, Object> node : prologTree.nodes()) {
      Node tmpNode;

      nodeConversionDictionary.put(node.get("node_id").toString(), index); // TODO: passare a un singolo dizionario
      if (node.get("start").toString().equals("1")) {
        tmpNode = new Input(index);
      } else if (node.get("type").toString().equals("operator")) {
        BaseOperator operator = BaseOperator.ADDITION;
        tmpNode = new OperatorNode(index, operator);
      } else if (node.get("type").toString().equals("variable")) {
        tmpNode = new Output(index);
      } else {
        throw new UnsupportedOperationException("Not acceptable type");
      }
      intermediateGraph.addNode(tmpNode);
      indexToNode.put(index,tmpNode);
      index++;
    }

    for (Graph.Arc<Map<String, Object>> arc : prologTree.arcs()) {
      intermediateGraph.setArcValue(indexToNode.get(nodeConversionDictionary.get(arc.getSource().get("node_id"))), indexToNode.get(nodeConversionDictionary.get(arc.getTarget().get("node_id"))), OperatorGraph.NON_VALUED_ARC);
    }


    System.out.println("\n\nIntermediate graph nodes:\n" + intermediateGraph.nodes());


    OperatorGraph resultingGraph = new OperatorGraph(intermediateGraph);
//    throw new UnsupportedOperationException();

//    return resultingGraph;
  }


  public static void main(String[] args) {

    //// Domain
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");

    // Generate graph
    PrologGraph binaryTree = TreeAnalysis.generateBinaryTreeGraph(15, domainDefinition);
    System.out.println("binTree nodes: " + binaryTree.nodes());
    System.out.println("binTree arcs: " + binaryTree.nodes());


    convert(binaryTree);

  }


}


