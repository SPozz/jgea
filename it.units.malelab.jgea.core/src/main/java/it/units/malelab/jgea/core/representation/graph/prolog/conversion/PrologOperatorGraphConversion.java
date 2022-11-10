package it.units.malelab.jgea.core.representation.graph.prolog.conversion;

import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.Node;
import it.units.malelab.jgea.core.representation.graph.numeric.Constant;
import it.units.malelab.jgea.core.representation.graph.numeric.Input;
import it.units.malelab.jgea.core.representation.graph.numeric.Output;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.BaseOperator;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.OperatorGraph;
import it.units.malelab.jgea.core.representation.graph.numeric.operatorgraph.OperatorNode;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.analysis.TreeAnalysis;

import java.util.*;

public class PrologOperatorGraphConversion {

  public static OperatorGraph convert(PrologGraph prologTree) {
    // convert Prolog Tree into Graph<Node, NonValuedArc>

    LinkedHashGraph<Node, OperatorGraph.NonValuedArc> intermediateGraph = new LinkedHashGraph<>();
    int index = 0;
    LinkedHashMap<String, Node> idToNode = new LinkedHashMap<>();

    BaseOperator[] baseOperators = new BaseOperator[]{BaseOperator.ADDITION, BaseOperator.DIVISION, BaseOperator.MULTIPLICATION, BaseOperator.SUBTRACTION};

    for (Map<String, Object> node : prologTree.nodes()) {
      Node tmpNode;

      if (node.get("start").toString().equals("1")) {
        tmpNode = new Input(index);
      } else if (node.get("type").toString().equals("operator")) {
        String prologOperator = node.get("value").toString();
        prologOperator = prologOperator.replace("'", "");
        tmpNode = new OperatorNode(index, baseOperators[0]);
        int debugger = 0;
        for (BaseOperator operator : baseOperators) {
          if (operator.toString().equals(prologOperator)) {
            tmpNode = new OperatorNode(index, operator);
            break;
          }
          debugger++;
        }
        if (debugger >= baseOperators.length )
          throw new UnsupportedOperationException("operator value not matching baseOperator values");
      } else if (node.get("type").toString().equals("variable")) {
//        String valueString = node.get("value").toString();
//        double value = Double.parseDouble(valueString);
//        tmpNode = new Constant(index,value); //NO. constants can't have predecessors

        tmpNode = new Output(index);

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

//    throw new UnsupportedOperationException();

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
    PrologGraph binaryTree = TreeAnalysis.generateBinaryTreeGraph(15, domainDefinition, operatorValues);
//    System.out.println("\nbinary Prolog graph.\nNodes: " + binaryTree.nodes());
//    System.out.println("size before: " + binaryTree.size());

    OperatorGraph convertedGraph = convert(binaryTree);
//    System.out.println("\nconverted graph.\nnInputs (= nStart): " + convertedGraph.nInputs() + "\nnOutputs (= nVariables): " + convertedGraph.nOutputs());
//    System.out.println("size after:  " + convertedGraph.size());
    System.out.println(convertedGraph);


  }

}


