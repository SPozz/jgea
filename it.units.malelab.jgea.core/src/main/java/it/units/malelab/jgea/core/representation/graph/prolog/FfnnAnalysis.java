package it.units.malelab.jgea.core.representation.graph.prolog;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.jpl7.Query;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
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

    int nNodes = random.nextInt(dimension / 3, dimension - 2);

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

      if (i == 1) {
        layer = 1;
      }

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

      int sourceLayer = 0;
      if (nodesAndLayers.size() > 1) {
        sourceLayer = random.nextInt(0, nodesAndLayers.size() - 1);
      }

      List<String> sourceRange = nodesAndLayers.get(sourceLayer);
      sourceID = sourceRange.get(random.nextInt(0, sourceRange.size()));

      List<String> targetRange = nodesAndLayers.get(sourceLayer + 1);
      targetID = targetRange.get(random.nextInt(0, targetRange.size()));

      edgeID = sourceID + targetID;
      if (edgeIDs.contains(edgeID)) {
        edgeID = sourceID + targetID + debugger;
        debugger += 1;
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
            "min_level(0) :- findall(L,layer(_,L),Layers), min_list(Layers,M).",
            "max_level(M) :- findall(L,layer(_,L),Layers), max_list(Layers,M).",
            "level(X) :- " +
                    "    float(X), max_level(Max), min_level(Min), " +
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

    String addEdge =
            "findall(ID,node_id(ID),Nodes)," +
                    "random_member(N,Nodes)," +
                    "layer(N,L)," +
                    "LL is L+1," +
                    "findall(ID2,layer(ID2,LL),NextLayerNodes)," +
                    "random_member(M,NextLayerNodes)," +
                    "( edge(N,M,_);" +
                    "    gensym(edg,E)," +
                    "    assert(edge_id(E))," +
                    "    assert(edge(N,M,E))," +
                    "    min_weight(MIN)," +
                    "    max_weight(MAX)," +
                    "    random_between(MIN,MAX,W)," +
                    "    assert(weight(E,W))" +
                    ").";
    operators.add(addEdge);
    operatorsLabels.add("addEdge");

    String addInitialLayer = "min_level(X)," +
            "Y is X -1," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "assert(layer(N,Y))," +
            "findall(Nod,layer(Nod,X),Nodes)," +
            "random_pair(M1,M2,Nodes)," +
            "gensym(edge_id,E1)," +
            "gensym(edge_id,E2)," +
            "assert(edge(N,M1,E1))," +
            "assert(edge(N,M2,E2))," +
            "max_weight(WMax)," +
            "min_weight(WMin)," +
            "random(WMin,WMax,W1)," +
            "random(WMin,WMax,W2)," +
            "assert(weight(E1,W1))," +
            "assert(weight(E2,W2)).";
    operators.add(addInitialLayer);
    operatorsLabels.add("addInitialLayer");


    String addFinalLayer = "max_level(X)," +
            "Y is X +1," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "assert(layer(N,Y))," +
            "findall(Nod,layer(Nod,X),Nodes)," +
            "random_pair(M1,M2,Nodes)," +
            "gensym(edge_id,E1)," +
            "gensym(edge_id,E2)," +
            "assert(edge(M1,N,E1))," +
            "assert(edge(M2,N,E2))," +
            "max_weight(WMax)," +
            "min_weight(WMin)," +
            "random(WMin,WMax,W1)," +
            "random(WMin,WMax,W2)," +
            "assert(weight(E1,W1))," +
            "assert(weight(E2,W2)).";
    operators.add(addFinalLayer);
    operatorsLabels.add("addFinalLayer");

    // Analysis:
    int nGraphs = 25;
    int nOperations = 40;

    int dimension = 10;
    List<LinkedHashMap<String, Object>> DataFrame10 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 25;
    List<LinkedHashMap<String, Object>> DataFrame25 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 40;
    List<LinkedHashMap<String, Object>> DataFrame40 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 55;
    List<LinkedHashMap<String, Object>> DataFrame55 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    String[] files = {"Dataframe10.csv", "Dataframe25.csv", "Dataframe40.csv", "Dataframe55.csv"};
    List<List<LinkedHashMap<String, Object>>> dfCollection = new ArrayList<>();
    dfCollection.add(DataFrame10);
    dfCollection.add(DataFrame25);
    dfCollection.add(DataFrame40);
    dfCollection.add(DataFrame55);

    for (int i = 0; i < dfCollection.size(); ++i) {
      String fileName = "FFNN" + files[i];
      List<LinkedHashMap<String, Object>> df = dfCollection.get(i);

      try {
        // create a writer
        Writer writer = Files.newBufferedWriter(Paths.get("C:\\Users\\Simone\\Desktop\\GitHub_Tesi\\jgea_data\\25x40\\" + fileName));

        // write CSV file
        CSVPrinter printer = CSVFormat.DEFAULT.withHeader("graph", "operator", "dimension", "executionTime").print(writer);

        for (LinkedHashMap<String, Object> map : df) {
          printer.printRecord(map.get("graph"), map.get("operator"), map.get("dimension"), map.get("executionTime"));
        }

        // flush the stream
        printer.flush();

        // close the writer
        writer.close();

      } catch (IOException ex) {
        ex.printStackTrace();
      }
    }


  }


}
