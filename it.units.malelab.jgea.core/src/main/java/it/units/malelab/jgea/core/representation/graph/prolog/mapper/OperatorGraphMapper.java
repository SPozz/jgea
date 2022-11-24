package it.units.malelab.jgea.core.representation.graph.prolog.mapper;

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
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;

import java.util.*;
import java.util.function.Function;

public class OperatorGraphMapper implements Function<PrologGraph, OperatorGraph> {

  @Override
  public OperatorGraph apply(PrologGraph prologTree) {
    LinkedHashGraph<Node, OperatorGraph.NonValuedArc> intermediateGraph = new LinkedHashGraph<>();
    int index = 1; //0 reserved for output node
    LinkedHashMap<String, Node> idToNode = new LinkedHashMap<>();
    final BaseOperator[] baseOperators = new BaseOperator[]{BaseOperator.ADDITION, BaseOperator.DIVISION, BaseOperator.MULTIPLICATION, BaseOperator.SUBTRACTION, BaseOperator.LOG};
    final String[] baseOperatorsString = new String[baseOperators.length];
    for (int i = 0; i < baseOperators.length; ++i) {
      baseOperatorsString[i] = baseOperators[i].toString();
    }
    final Output outputNode = new Output(0);
    intermediateGraph.addNode(outputNode);

    for (Map<String, Object> node : prologTree.nodes()) {
      Node tmpNode;
      if (node.get("type").toString().equalsIgnoreCase("operator")) {
        final String prologOperator = node.get("value").toString().replace("'", "");
        final int indexOfOperator = Arrays.asList(baseOperatorsString).indexOf(prologOperator);
        tmpNode = new OperatorNode(index, baseOperators[indexOfOperator]);
        if (node.get("start").toString().equals("1")) {
          idToNode.put(node.get("node_id").toString(), tmpNode);
          intermediateGraph.addNode(tmpNode);
          index++;
          intermediateGraph.setArcValue(tmpNode, outputNode, OperatorGraph.NON_VALUED_ARC);
          continue;
        }
      } else if (node.get("type").toString().equalsIgnoreCase("constant")) {
        final String valueString = node.get("value").toString();
        final double value = Double.parseDouble(valueString);
        tmpNode = new Constant(index, value);
      } else if (node.get("type").toString().equalsIgnoreCase("input")) {
        tmpNode = new Input(index);
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
    final List<String> domainDefinition = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");

    final List<String> structuralRules = Arrays.asList(
            "operator_val(+).",
            "operator_val(*).",
            "operator_val(-).",
            "operator_val(/).",
            "input_val(inp).",
            "constant_val(X) :- integer(X), X>=0, X< 10.",
            "start_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 0.",
            "node_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 1.",
            "operator_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 2.",
            "leaf_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 0.",
            "check_start :- findall(N,start(N,1), N),length(N,N1), N1 == 1.",
            "start_connected(N) :- start(N,1).",
            "start_connected(N) :- edge(N,X,_), start_connected(X).",
            "is_valid :- " +
                    "    check_start," +
                    "    foreach(findall(N,node_id(N),Node),maplist(start_connected,Node))," +
                    "    foreach(findall(T,(node_id(T),start(T,1)),T), maplist(start_outdegree,T))," +
                    "    foreach(findall(T,(node_id(T),start(T,0)),T), maplist(node_outdegree,T))," +
                    "    foreach(findall(O,type(O,operator),O), maplist(operator_indegree,O))," +
                    "    foreach(findall(V,type(V,input),V), maplist(leaf_indegree,V))," +
                    "    foreach(findall(C,type(C,constant),C), maplist(leaf_indegree,C)).");

    //// Operators
    List<String> operators = new ArrayList<>();
    String subTree = "findall(VV,(type(VV,input); type(VV,constant)),VAR)," +
            "random_member(V,VAR)," +
            "retract(value(V,_))," +
            "retract(type(V,_))," +
            "operator_val(OpVal)," +
            "assert(type(V,operator))," +
            "assert(value(V,OpVal))," +
            "gensym(nod,N1)," +
            "assert(node_id(N1))," +
            "(   maybe ->  assert(type(N1,input)); " +
            "    assert(type(N1,constant)) )," +
            "random_between(0,10,V1Val)," +
            "assert(value(N1,V1Val))," +
            "assert(start(N1,0))," +
            "gensym(nod,N2)," +
            "assert(node_id(N2))," +
            "(   maybe ->  assert(type(N2,input)); " +
            "    assert(type(N2,constant)) )," +
            "random_between(0,10,V2Val)," +
            "assert(value(N2,V2Val))," +
            "assert(start(N2,0))," +
            "gensym(edge,E1)," +
            "gensym(edge,E2)," +
            "assert(edge_id(E1))," +
            "assert(edge_id(E2))," +
            "assert(edge(N1,V,E1))," +
            "assert(edge(N2,V,E2)).";
    operators.add(subTree);

    String changeOperator = "findall(OP,type(OP,operator), Operators)," +
            "random_member(O, Operators)," +
            "retract(value(O,_))," +
            "findall(V,operator_val(V),Values)," +
            "random_member(X,Values)," +
            "assert(value(O,X))";
    operators.add(changeOperator);

    String changeConstant = "findall(CON,type(CON,constant), Constants)," +
            "random_member(O, Constants)," +
            "retract(value(O,_))," +
            "random_between(0,9,X)," +
            "assert(value(O,X)).";
    operators.add(changeConstant);

    String removeLeaves = "findall(VV,(node_id(VV),(type(VV,variable); type(VV,input)),LeavesID)," +
            "random_member(V,LeavesID)," +
            "edge(V,T,ID)," +
            "retract(edge_id(ID))," +
            "retract(edge(V,T,ID))," +
            "retract(node_id(V))," +
            "retract(start(V,0))," +
            "retract(type(V,_))," +
            "edge(S,T,ID2)," +
            "retract(node_id(S))," +
            "retract(start(S,0))," +
            "retract(type(S,_))," +
            "retract(value(S,_))," +
            "retract(edge_id(ID2))," +
            "retract(edge(S,T,ID2))," +
            "retract(type(T,_))," +
            "retract(value(T,_))," +
            "random_between(0,10,Val)," +
            "assert(value(T,Val))," +
            "(maybe -> assert(type(T,variable));" +
            "assert(type(T,input)) ).";
    operators.add(removeLeaves);

    String swapEdges = "findall(VV,(type(VV,variable); type(VV,input) ),Leaves)," +
            "random_member(V1,Leaves)," +
            "random_member(V2,Leaves)," +
            "edge(V1,T1,Id1)," +
            "edge(V2,T2,Id2)," +
            "retract(edge(V1,T1,Id1))," +
            "retract(edge(V2,T2,Id2))," +
            "assert(edge(V1,T2,Id1))," +
            "assert(edge(V2,T1,Id2)).";
    operators.add(swapEdges);

//  // Export analysis of Factory
    PrologGraph origin = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("start", 1);
    node1.put("type", "operator");
    node1.put("value", "+");
    LinkedHashMap<String, Object> node2 = new LinkedHashMap<>();
    node2.put("node_id", "second");
    node2.put("start", 0);
    node2.put("type", "constant");
    node2.put("value", 1.0d);
    LinkedHashMap<String, Object> node3 = new LinkedHashMap<>();
    node3.put("node_id", "third");
    node3.put("start", 0);
    node3.put("type", "input");
    node3.put("value", "inp");
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "firstEdge");
    LinkedHashMap<String, Object> edge2 = new LinkedHashMap<>();
    edge2.put("edge_id", "secondEdge");
    origin.addNode(node1);
    origin.addNode(node2);
    origin.addNode(node3);
    origin.setArcValue(node2, node1, edge1);
    origin.setArcValue(node3, node1, edge2);

    List<PrologGraph> graphs = new PrologGraphFactory(9, 9, origin, operators, domainDefinition, structuralRules).build(3, new Random());

    List<OperatorGraph> opGraphs = new ArrayList<>();
    for (PrologGraph graph : graphs) {
      opGraphs.add(new OperatorGraphMapper().apply(graph));
    }


    int counter = 0;
    for (int i = 0; i < graphs.size(); ++i) {
      PrologGraph graph = graphs.get(i);
      if ((graph.size() + 2) != opGraphs.get(i).size()) { //+2 since add operator and arc
        counter++;
      }
    }
    System.out.println("\nCounter = " + counter);


  }


}


