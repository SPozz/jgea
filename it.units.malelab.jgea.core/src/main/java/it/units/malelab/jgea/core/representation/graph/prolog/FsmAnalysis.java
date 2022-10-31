package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.Query;

import java.time.Duration;
import java.time.Instant;
import java.util.*;

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

  static List<LinkedHashMap<String, Object>> analysis(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();

    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      BasicGraphsAnalysis.resetProlog(factsNames);
      graph = generateGraph(dimension, domainDefinition, structuralRules);

      for (int j = 0; j < nOperations; ++j) {
        LinkedHashMap<String, Object> observation = new LinkedHashMap<>();
        Random rand = new Random();
        int randomIndex = rand.nextInt(0, operators.size());
        String randomOperator = operators.get(randomIndex);
        Instant startingInstant = Instant.now();
        int previousDimension = graph.nodes().size() + graph.arcs().size();
        graph = PrologGraphUtils.applyOperator(randomOperator, graph, domainDefinition, structuralRules);
        Instant endInstant = Instant.now();
        observation.put("graph", i);
        observation.put("operator", operatorsLabels.get(randomIndex));
        observation.put("dimension", previousDimension);
        observation.put("executionTime", Duration.between(startingInstant, endInstant).toNanos() / 1000000000d);

        DataFrame.add(observation);
      }
    }

    return DataFrame;
  }

  public static void main(String[] args) {
    // Subset definition:
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");

    List<String> factsNames = Arrays.asList("node_id/1", "start/2","accepting/2", "edge_id/1", "edge/3", "input/2");

    List<String> structuralRules = Arrays.asList("n_input(2).",
            "input_domain(X) :- n_input(MAX), integer(X), X =< MAX -1, X >= 0.",
            "accepting_domain(X) :- integer(X), X =< 1, X >= 0.",
            "start_domain(X) :- integer(X), X =< 1, X >= 0.",
            "size([], 0) :- true.",
            "size([_|Xs], N) :- size(Xs, N1), plus(N1,1,N).",
            "check_start :- findall(N,start(N,1), N), size(N,N1), N1 == 1.",
            "check_out(S) :- findall(S,edge(S,_,_),RES), size(RES,N), N == 2.",
            "is_valid :- check_start, foreach(findall(N,node_id(N),N), maplist(check_out,N))."
    );

    // Operators:
    List<String> operators = new ArrayList<>();
    List<String> operatorsLabels = new ArrayList<>();

    String addLegalNode = "gensym(nod,N)," + //Changed from general case
            "assert(node_id(N))," +
            "random_between(0,1,INT)," +
            "assert(accepting(N,INT))," +
            "assert(start(N,0))," +
            "findall(X,node_id(X), IDs)," +
            "  random_member(S,IDs)," +
            "  gensym(edge,E), " +
            "  assert(edge_id(E))," +
            "  assert(edge(N,S,E))," +
            "  assert(input(E,X))" +
            "  random_member(S,IDs)," +
            "  gensym(edge,E), " +
            "  assert(edge_id(E))," +
            "  assert(edge(N,S,E))," +
            "  assert(input(E,X))" +
            ".";
    operatorsLabels.add("addLegalNode");
    operators.add(addLegalNode);

    String changeStart = "findall(N,start(N,0),NonStartBefore)," +
            "findall(O,start(O,1),StartBefore)," +
            "random_member(X,NonStartBefore)," +
            "random_member(Y,StartBefore)," +
            "retract(start(Y,1))," +
            "assert(start(Y,0))," +
            "retract(start(X,0))," +
            "assert(start(X,1)).";
    operatorsLabels.add("changeStart");
    operators.add(changeStart);

    String changeAcceptingValue = "findall(N,node_id(N), Nodes)," +
            "random_member(M,Nodes)," +
            "retract(accepting(M,_))," +
            "random_between(0,1,V)," +
            "assert(accepting(M,V))";
    operatorsLabels.add("changeAcceptingValue");
    operators.add(changeAcceptingValue);

    String changeTarget = "findall(E,edge_id(E),EdgeIDs)," +
            "random_member(F,EdgeIDs)," +
            "findall(N,node_id(N),NodeIDs)," +
            "random_member(M,NodeIDs)," +
            "edge(S,_,F)," +
            "(   edge(S,M,_) ->   true;" +
            "retract(edge(S,_,F))," +
            "assert(edge(S,M,F))" +
            "    ).";
    operatorsLabels.add("changeTarget");
    operators.add(changeTarget);

    String changeInputOrder = "findall(M,node_id(M),NodeIDs)," +
            "random_member(N,NodeIDs)," +
            "edge(N,T0,ID0), input(ID0,0)," +
            "edge(N,T1,ID1), input(ID1,1)," +
            "retract(input(ID0,0))," +
            "retract(input(ID1,1))," +
            "assert(input(ID0,1))," +
            "assert(input(ID1,0)).";
    operatorsLabels.add("changeInputOrder");
    operators.add(changeInputOrder);



    // Analysis:
    int nGraphs = 1;
    int nOperations = 5;

    int dimension = 10;
    List<LinkedHashMap<String, Object>> DataFrame10 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);


  }


}
