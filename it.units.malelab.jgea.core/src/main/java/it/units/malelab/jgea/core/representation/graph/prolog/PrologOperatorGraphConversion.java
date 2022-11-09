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
        for (BaseOperator operator : baseOperators) { //can be done better(?)
          if (operator.toString().equals(prologOperator)) {
            tmpNode = new OperatorNode(index, operator);
            break;
          }
        }
      } else if (node.get("type").toString().equals("variable")) {
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

    List<String> structuralRules = Arrays.asList(
            "variable_val(X) :- integer(X), X>= 0, X < 10.",
            "operator_val(+).",
            "operator_val(*).",
            "operator_val(-).",
            "operator_val(/).",
            "start_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 0.",
            "node_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 1.",
            "operator_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 2.",
            "variable_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 0.",
            "check_start :- findall(N,start(N,1), N),length(N,N1), N1 == 1.",
            "start_connected(N) :- start(N,1).",
            "start_connected(N) :- edge(X,N,_), start_connected(X).",
            "is_valid :- " +
                    "    check_start," +
                    "    foreach(findall(N,node_id(N),Node),maplist(start_connected,Node))," +
                    "    foreach(findall(T,(node_id(T),start(T,1)),T), maplist(start_indegree,T))," +
                    "    foreach(findall(T,(node_id(T),start(T,0)),T), maplist(node_indegree,T))," +
                    "    foreach(findall(O,type(O,operator),O), maplist(operator_outdegree,O))," +
                    "    foreach(findall(V,type(V,variable),V), maplist(variable_outdegree,V)).");


    // Generate graph
    PrologGraph binaryTree = TreeAnalysis.generateBinaryTreeGraph(15, domainDefinition);
    System.out.println("\nbinary Prolog graph.\nNodes: " + binaryTree.nodes());
    System.out.println("size before: " + binaryTree.size());


    // Conversion
    List<String> operatorValues = new ArrayList<>();
    for (String rule : structuralRules) {
      if (!rule.contains("operator_val"))
        continue;
      rule = rule.replace("operator_val(", "");
      rule = rule.replace(").", "");
      operatorValues.add(rule);
    }

    BaseOperator[] baseOperators = new BaseOperator[]{BaseOperator.ADDITION, BaseOperator.DIVISION, BaseOperator.MULTIPLICATION, BaseOperator.SUBTRACTION};

    if (operatorValues.size() != baseOperators.length) {
      throw new UnsupportedOperationException("Different domains definition for operator value");
    }
    Collections.sort(operatorValues);



    OperatorGraph convertedGraph = convert(binaryTree);
    System.out.println("\nconverted graph.\nnInputs (= nStart): " + convertedGraph.nInputs() + "\nnOutputs (= nVariables): " + convertedGraph.nOutputs());
    System.out.println("size after:  " + convertedGraph.size());
    System.out.println(convertedGraph);

  }


}


