package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.Query;

import java.time.Duration;
import java.time.Instant;
import java.util.*;

public class BasicGraphs {

  static PrologGraph generateGraph (int dimension, List<String> domainDefinition, List<String> structuralRules){
    Random random = new Random();

    List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<String> colours = Arrays.asList("red", "blue", "yellow", "orange", "green");
    List<String> node;
    List<String> edge;
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> nodesIDS = new ArrayList<>();


    double value;
    String nodeID;
    String source;
    String target;
    String edgeID;
    String colour;
    int nNodes = random.nextInt(dimension / 2, dimension - 1);

    for (int i = 0; i < nNodes; ++i) {
      value = random.nextDouble(0, 1);
      nodeID = alphabet.get(random.nextInt(0, alphabet.size()));
      node = Arrays.asList("node_id(" + nodeID + ")", "attribute(" + nodeID + "," + value + ")");
      nodesIDS.add(nodeID);
      allNodes.add(node);
    }

    for (int j = 0; j < (dimension - nNodes); ++j) {
      source = nodesIDS.get(random.nextInt(0, nodesIDS.size()));
      target = nodesIDS.get(random.nextInt(0, nodesIDS.size()));
      edgeID = source + target;
      colour = colours.get(random.nextInt(0, colours.size()));
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "colour(" + edgeID + "," + colour + ")");
      allEdges.add(edge);
    }

    List<String> graphDescription = new ArrayList<>();
    for (int j = 0; j < 2; ++j) {
      for (List<String> oneNode : allNodes) {
        graphDescription.add(oneNode.get(j));
      }
    }

    for (int j = 0; j < 3; ++j) {
      for (List<String> oneEdge : allEdges) {
        graphDescription.add(oneEdge.get(j));
      }
    }

    for (String fact : graphDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    for (String rule : structuralRules) {
      rule = rule.replace(".", "");
      rule = rule.replace(" ", "");
      Query.hasSolution("assert((" + rule + "))");
    }

    return PrologGraphUtils.buildGraph(domainDefinition);
  }

  public static void main(String[] args) {
    // Operators' definition
    String operatorAddNodeWithAttribute = "gensym(nod,X), assert(node_id(X)), random(V), attribute_value(V), assert(attribute(X,V)).";

    String operatorAddEdgeWithAttribute = "findall(N,node_id(N), Nodes)," +
            "random_member(Source, Nodes)," +
            "random_member(Target, Nodes), " +
            "gensym(edg,E)," +
            "assert(edge_id(E))," +
            "assert(edge(Source,Target,E))," +
            "findall(C,colour_value(C),CDomain)," +
            "random_member(RED,CDomain)," +
            "assert(colour(E,RED)).";

    String operatorRemoveEdgeWithAttribute = "findall(EID,edge_id(EID),Ids)," +
            "random_member(Removable,Ids)," +
            "retract(edge_id(Removable))," +
            "retract(colour(Removable,_))," +
            "retract(edge(_,_,Removable)).";

    String operatorIntermediateNodeWithAttributes = "findall((S,T,I),edge(S,T,I),Edges)," +
            "random_member((N1,N2,ID),Edges)," +
            "retract(edge_id(ID))," +
            "retract(edge(N1,N2,ID))," +
            "retract(colour(ID,_))," +
            "gensym(edg,E1)," +
            "gensym(edg,E2)," +
            "assert(edge_id(E1))," +
            "assert(edge_id(E2))," +
            "findall(C,colour_value(C),CDomain)," +
            "random_member(C1,CDomain)," +
            "random_member(C2,CDomain)," +
            "assert(colour(E1,C1))," +
            "assert(colour(E2,C2))," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "random(V), attribute_value(V), assert(attribute(X,V))," + //if with attribute
            "assert(edge(N1,N,E1))," +
            "assert(edge(N,N2,E2)).";

    String operatorPerturbValue = "findall(X,node_id(X),Nodes)," +
            "random_member(N,Nodes)," +
            "attribute(N,V)," +
            "random(R)," +
            "New_Val is R*V," +
            "(attribute_value(New_Val) ->  retract(attribute(N,V))," +
            "                    assertz(attribute(N,New_Val));" +
            "    true).";

    String operatorModifyEdgeValue = "findall(ID,edge_id(ID),IDs)," +
            "random_member(E,IDs)," +
            "findall(C,colour_value(C),Colours)," +
            "random_member(V,Colours)," +
            "retract(colour(E,_))," +
            "assert(colour(E,V)).";

    String operatorRemoveNode = "findall(NId,node_id(NId),Nodes),random_member(N,Nodes),\tfindall(E,edge(_,N,E),ID_in),findall(E,edge(N,_,E),ID_out),retract_list(ID_in,edge_id),retract_list(ID_out,edge_id),retractall(edge(_,N,_)),retractall(edge(N,_,_)),retract(node_id(N)).";

    // Subset of the graph:
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.", ":- dynamic attribute/2.", ":- dynamic edge_id/1.", ":- dynamic edge/3.", ":- dynamic colour/2.");

    List<String> structuralRules = Arrays.asList("is_valid :- true.",
            "retract_list([X | Xs], P) :- " +
                    "        Z =.. [P, X], retract(Z), retract_list(Xs, P).",
            "retract_list([], _) :- true.",
            "attribute_value(X) :- float(X), X =< 1, X >= 0.",
            "colour_value(red).",
            "colour_value(blue).",
            "colour_value(green).",
            "colour_value(orange).",
            "colour_value(yellow)."
    );




    PrologGraph graph = generateGraph(10,domainDefinition,structuralRules);
    System.out.println(graph.nodes());
    System.out.println(graph.arcs());


    Instant startingInstant = Instant.now();
    PrologGraphUtils.applyOperator(operatorAddNodeWithAttribute, graph,domainDefinition, structuralRules);
    System.out.println("execution time: "+ Duration.between(startingInstant,Instant.now()).toNanos()/ 1000000000d);


    //  TODO: Generalize and collect data






  }
}
