package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.Query;

import java.time.Duration;
import java.time.Instant;
import java.util.*;

public class FfnnAnalysis {

  static PrologGraph generateGraph(int dimension, List<String> domainDefinition) {
    Random random = new Random();

    List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<String> node;
    List<String> edge;
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();


    int layer;
    String nodeID;
    String edgeID;

    List<String> indexList = new ArrayList<>();

    int MaxRecursion = 100;

    int nNodes = random.nextInt(dimension / 2 - 1, dimension - 1);

    List<List<String>> nodesAndLayers = new ArrayList<>();
    for (int i = 0; i < nNodes; ++i) {
      nodesAndLayers.add(new ArrayList<>());
    }

    int maxLayer = 0;

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

      layer = random.nextInt(0, maxLayer + 1);
      if (layer == maxLayer) {
        maxLayer += 1;
      }
      node = Arrays.asList("node_id(" + nodeID + ")", "layer(" + nodeID + "," + layer + ")");
      nodesAndLayers.get(layer).add(nodeID);

      allNodes.add(node);
    }

    for (int reverseIndex = nodesAndLayers.size() - 1; reverseIndex >= 0; reverseIndex--) {
      if (nodesAndLayers.get(reverseIndex).isEmpty()) {
        nodesAndLayers.remove(reverseIndex);
      } else {
        break;
      }
    }


    List<String> edgeIDs = new ArrayList<>();
    int debugger = 1;

    for (int j = 0; j < (dimension - nNodes); ++j) {
      String sourceID;
      String targetID;
      double weight;


      int sourceLayer = random.nextInt(0, nodesAndLayers.size() - 1);

      List<String> sourceRange = nodesAndLayers.get(sourceLayer);
      sourceID = sourceRange.get(random.nextInt(0, sourceRange.size()));

      List<String> targetRange = nodesAndLayers.get(sourceLayer + 1);
      targetID = targetRange.get(random.nextInt(0, targetRange.size()));

      edgeID = sourceID + targetID;
      if (edgeIDs.contains(edgeID)) {
        edgeID = sourceID + targetID + debugger;
        debugger += 1;
        continue;
      }
      edgeIDs.add(edgeID);

      weight = random.nextDouble(0, 1);
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + sourceID + "," + targetID + "," + edgeID + ")", "weight(" + edgeID + "," + weight + ")");
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

    return PrologGraphUtils.buildGraph(domainDefinition);
  }

  static List<LinkedHashMap<String, Object>> analysis(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();
    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      BasicGraphsAnalysis.resetProlog(factsNames);
      graph = generateGraph(dimension, domainDefinition);

      for (int j = 0; j < nOperations; ++j) {
        LinkedHashMap<String, Object> observation = new LinkedHashMap<>();
        Random rand = new Random();
        int randomIndex = rand.nextInt(0, operators.size());
        String randomOperator = operators.get(randomIndex);
        int previousDimension = graph.nodes().size() + graph.arcs().size();
        Instant startingInstant = Instant.now();
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
    //// Domain
    List<String> domainDefinition = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic layer/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic weight/2."
    );

    List<String> structuralRules = Arrays.asList(
            "max_weight(1.0).",
            "min_weight(0.0).",
            "min_level(0).",
            "max_level(M) :- findall(L,layer(_,L),Layers), max_list(Layers,M).",
            "level(X) :- " +
                    "    float(X), max_level(Max), min_level(Min), \n" +
                    "                            X =< Max, X >= Min.",
            "weight_val(X) :- " +
                    "    max_weight(Max), min_weight(Min),float(X), " +
                    "                            X < Max, X >= Min.",
            "edg_consist_from_node(N) :-" +
                    "  layer(N,M)," +
                    "  findall(L,(layer(T,L),edge(N,T,_), L =\\= M + 1),RES)," +
                    "                                   length(RES,Z), Z == 0.",
            "is_valid :- " +
                    "    foreach( findall(N,node_id(N),N)," +
                    "        maplist(edg_consist_from_node,N)" +
                    "    )."
    );

    List<String> factsNames = Arrays.asList("node_id/1", "layer/2", "edge_id/1", "edge/3", "weight/2");


    //// Operators
    List<String> operators = new ArrayList<>();
    List<String> operatorsLabels = new ArrayList<>();

    String addEdge = "gensym(edg,E)," +
            "assert(edge_id(E))," +
            "findall(ID,node_id(ID),Nodes)," +
            "random_member(N,Nodes)," +
            "layer(N,L)," +
            "LL is L+1," +
            "findall(ID2,layer(ID2,LL),NextLayerNodes)," +
            "random_member(M,NextLayerNodes)," +
            "( edge(N,M,_) -> retract(edge_id(E));" +
            "    assert(edge(N,M,E))," +
            "    min_weight(MIN)," +
            "    max_weight(MAX)," +
            "    random_between(MIN,MAX,W)," +
            "    assert(weight(E,W))" +
            ").";
    operators.add(addEdge);
    operatorsLabels.add("addEdge");


    // TEST
    PrologGraph graph = generateGraph(20, domainDefinition);
    System.out.println(graph.nodes());
    System.out.println(graph.arcs());

    analysis(10,1,25,operators,operatorsLabels,factsNames,domainDefinition,structuralRules);



  }


}
