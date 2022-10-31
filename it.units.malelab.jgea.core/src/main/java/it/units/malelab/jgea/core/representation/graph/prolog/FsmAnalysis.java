package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.Query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class FsmAnalysis {

  static PrologGraph generateGraph(int dimension, List<String> domainDefinition, List<String> structuralRules) {
    Random random = new Random();

    List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<String> node;
    List<String> edge;
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> nodesIDS = new ArrayList<>();


    String nodeID;
    String source;
    String target;
    String edgeID;
    List<String> indexList = new ArrayList<>();

    int MaxRecursion = 100;

    int nNodes = dimension / 3;

    for (int i = 0; i < nNodes; ++i) {
      int index = random.nextInt(0, alphabet.size());
      int check = 0;
      while (indexList.contains(Integer.toString(index)) & check <= MaxRecursion) {
        index = random.nextInt(0, alphabet.size());
        ++check;
      }
      if (check == MaxRecursion) {
        continue;
      }
      indexList.add(Integer.toString(index));
      nodeID = alphabet.get(index);
      nodesIDS.add(nodeID);

      int start = 0;
      if (i == 0) {
        start = 1;
      }
      int accepting = random.nextInt(0, 2);

      node = Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "accepting(" + nodeID + "," + accepting + ")");
      allNodes.add(node);
    }

    List<String> edgeIDs = new ArrayList<>();
    for (String oneNode : nodesIDS) {
      for (int h = 0; h < 2; ++h) {
        source = oneNode;
        target = nodesIDS.get(random.nextInt(0, nodesIDS.size()));
        edgeID = source + target;

        int iteration = 0;
        while (edgeIDs.contains(edgeID) & iteration < MaxRecursion) {
          target = nodesIDS.get(random.nextInt(0, nodesIDS.size()));
          edgeID = source + target;
          iteration++;
        }

        edgeIDs.add(edgeID);

        int input = random.nextInt(0, 2);
        edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "input(" + edgeID + "," + input + ")");
        allEdges.add(edge);
      }
    }

    List<String> graphDescription = new ArrayList<>();
    for (int j = 0; j < 3; ++j) {
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
    // Subset definition:
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");

    List<String> structuralRules = Arrays.asList("n_input(2).",
            "input_domain(X) :- n_input(MAX), integer(X), X =< MAX -1, X >= 0.",
            "accepting_domain(X) :- integer(X), X =< 1, X >= 0.",
            "start_domain(X) :- integer(X), X =< 1, X >= 0.",
            "size([], 0).",
            "size([_|Xs], N) :- size(Xs, N1), N is N1 + 1.",
            "check_start :- findall(N,start(N,1), N), size(N,N1), N1 == 1.",
            "check_out(S) :- findall(S,edge(S,_,_),RES), size(RES,N), N == 2.",
            "is_valid :- check_start, foreach(findall(N,node_id(N),N), maplist(check_out,N))."
    );

    PrologGraph grafo = generateGraph(9, domainDefinition, structuralRules);
    System.out.println(grafo.nodes());


  }


}
