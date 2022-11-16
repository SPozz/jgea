package it.units.malelab.jgea.core.representation.graph.prolog.analysis;

import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphUtils;
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

  public static PrologGraph generateFfnnGraph(int dimension, List<String> domainDefinition) {
    final Random random = new Random();
    final List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();

    // Attributes for graph
    int nNodeAttributes = 2; // including ID
    int nArcAttributes = 3; // including id and edge
    if ((nArcAttributes + nNodeAttributes) != domainDefinition.size()) {
      throw new UnsupportedOperationException("Wrong definition of number of attributes");
    }

    List<String> indexList = new ArrayList<>();
    int debuggerID = 1;

    int nNodes = random.nextInt(dimension / 3, dimension - 2);

    List<List<String>> nodesAndLayers = new ArrayList<>();
    for (int i = 0; i < nNodes; ++i) {
      nodesAndLayers.add(new ArrayList<>());
    }

    int maxLayer = 0;

    for (int i = 0; i < nNodes; ++i) {
      int index = random.nextInt(0, alphabet.size());
      String nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debuggerID;
        debuggerID++;
      }
      indexList.add(Integer.toString(index));

      int layer = random.nextInt(0, maxLayer + 1);

      if (i == 1) {
        layer = 1;
      }

      if (layer == maxLayer) {
        maxLayer += 1;
      }
      nodesAndLayers.get(layer).add(nodeID);
      allNodes.add(Arrays.asList("node_id(" + nodeID + ")", "layer(" + nodeID + "," + layer + ")"));
    }

    for (int reverseIndex = nodesAndLayers.size() - 1; reverseIndex >= 0; reverseIndex--) {
      if (nodesAndLayers.get(reverseIndex).isEmpty()) {
        nodesAndLayers.remove(reverseIndex);
      } else {
        break;
      }
    }


    List<String> edgeIDs = new ArrayList<>();
    int recursion = 1;
    int maxRecursion = 100;

    for (int j = 0; j < (dimension - nNodes); ++j) {
      if (recursion >= maxRecursion) { // limit on recursion
        break;
      }
      int sourceLayer = 0;
      if (nodesAndLayers.size() > 1) {
        sourceLayer = random.nextInt(0, nodesAndLayers.size() - 1);
      }

      List<String> sourceRange = nodesAndLayers.get(sourceLayer);
      String sourceID = sourceRange.get(random.nextInt(0, sourceRange.size()));

      List<String> targetRange = nodesAndLayers.get(sourceLayer + 1);
      String targetID = targetRange.get(random.nextInt(0, targetRange.size()));

      String edgeID = sourceID + targetID;
      if (edgeIDs.contains(edgeID)) { //if edge already exists there, re-do. up to limit on recursion
        --j;
        recursion++;
        continue;
      }
      edgeIDs.add(edgeID);
      recursion = 1;

      double weight = random.nextDouble(0, 1);
      allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + sourceID + "," + targetID + "," + edgeID + ")", "weight(" + edgeID + "," + weight + ")"));
    }

    List<String> graphDescription = new ArrayList<>();
    for (int j = 0; j < nNodeAttributes; ++j) {
      for (List<String> oneNode : allNodes) {
        graphDescription.add(oneNode.get(j));
      }
    }

    for (int j = 0; j < nArcAttributes; ++j) {
      for (List<String> oneEdge : allEdges) {
        graphDescription.add(oneEdge.get(j));
      }
    }

    for (String fact : graphDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    return PrologGraphUtils.buildGraph(domainDefinition);
  }

  private static List<LinkedHashMap<String, Object>> analyseFfnnGeneration(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();
    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      BasicGraphsAnalysis.resetProlog(factsNames);
      graph = generateFfnnGraph(dimension, domainDefinition);

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

  private static void exportFfnnAnalysis(List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    int nGraphs = 25;
    int nOperations = 40;

    int dimension = 10;
    List<LinkedHashMap<String, Object>> DataFrame10 = analyseFfnnGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 25;
    List<LinkedHashMap<String, Object>> DataFrame25 = analyseFfnnGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 40;
    List<LinkedHashMap<String, Object>> DataFrame40 = analyseFfnnGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 55;
    List<LinkedHashMap<String, Object>> DataFrame55 = analyseFfnnGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

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

  public static void main(String[] args) {
    //// Domain
    final List<String> domainDefinition = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic layer/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic weight/2."
    );

    final List<String> structuralRules = Arrays.asList(
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
            "random_pair(Z1,Z2,List) :-" +
                    "    random_member(Z1,List)," +
                    "    random_member(Z2,List)," +
                    "    Z1 \\== Z2.",
            "is_valid :- " +
                    "    foreach( findall(N,node_id(N),N)," +
                    "        maplist(edg_consist_from_node,N)" +
                    "    )."
    );

    final List<String> factsNames = Arrays.asList("node_id/1", "layer/2", "edge_id/1", "edge/3", "weight/2");


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

    String addNode = "max_level(Max)," +
            "min_level(Min)," +
            "random_between(Min,Max,L)," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "assert(layer(N,L)).";
    operators.add(addNode);
    operatorsLabels.add("addNode");

    String addNodeEdge = "max_level(Max)," +
            "min_level(Min)," +
            "random_between(Min,Max,L)," +
            "findall(M,node_id(M),Nodes)," +
            "max_weight(MaxW)," +
            "min_weight(MinW)," +
            "random_between(MinW,MaxW,W)," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "assert(layer(N,L))," +
            "random_member(X,Nodes)," +
            "gensym(edg,E)," +
            "assert(edge_id(E))," +
            "assert(edge(X,N,E))," +
            "assert(weight(E,W)).";
    operators.add(addNodeEdge);
    operatorsLabels.add("addNodeEdge");

//    //Export CSV
    exportFfnnAnalysis(operators, operatorsLabels, factsNames, domainDefinition, structuralRules);


  }

}
