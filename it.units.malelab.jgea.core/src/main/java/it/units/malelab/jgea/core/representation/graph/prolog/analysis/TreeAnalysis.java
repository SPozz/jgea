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

public class TreeAnalysis {

  public static PrologGraph generateBinaryTreeGraph(int dimension, List<String> domainDefinition, List<String> operatorValues) {
    final Random random = new Random();
    final List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    final List<String> typeValue = Arrays.asList("operator", "variable");
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();

    // Attributes for graph
    final int nNodeAttributes = 4; // including ID
    final int nArcAttributes = 2; // including id and edge
    if ((nArcAttributes + nNodeAttributes) != domainDefinition.size()) {
      throw new UnsupportedOperationException("Wrong definition of number of attributes");
    }
    List<String> indexList = new ArrayList<>();
    int debuggerID = 1;
    int tmpDimension = 1;

    // first node
    int start = 1;
    String type = "operator";
    String value = operatorValues.get(random.nextInt(0, operatorValues.size()));
    int index = random.nextInt(0, alphabet.size());
    indexList.add(Integer.toString(index));
    String nodeID = alphabet.get(index);
    allNodes.add(Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")"));
    tmpDimension += 1;

    List<String> invalidNodes = new ArrayList<>();
    invalidNodes.add(nodeID);

    start = 0;
    while (tmpDimension < dimension - 1) {
      String targetID = invalidNodes.get(random.nextInt(0, invalidNodes.size()));

      // first new node
      index = random.nextInt(0, alphabet.size());
      nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debuggerID;
        debuggerID++;
      }
      indexList.add(Integer.toString(index));
      type = "operator";
      value = operatorValues.get(random.nextInt(0, operatorValues.size()));
      allNodes.add(Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")"));
      tmpDimension++;
      invalidNodes.add(nodeID);

      // add edge
      String edgeID = nodeID + targetID; //always new since node is new
      allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + nodeID + "," + targetID + "," + edgeID + ")"));
      tmpDimension++;

      // second new node
      index = random.nextInt(0, alphabet.size());
      nodeID = alphabet.get(index);
      if (indexList.contains(Integer.toString(index))) {
        nodeID += debuggerID;
        debuggerID++;
      }
      indexList.add(Integer.toString(index));
      type = typeValue.get(random.nextInt(0, typeValue.size()));
      if (type.equals("operator")) {
        invalidNodes.add(nodeID);
        value = operatorValues.get(random.nextInt(0, operatorValues.size()));
      } else {
        value = Integer.toString(random.nextInt(0, 10));
      }
      allNodes.add(Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")"));
      tmpDimension++;

      // add edge
      edgeID = nodeID + targetID; //always new since node is new
      allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + nodeID + "," + targetID + "," + edgeID + ")"));
      tmpDimension++;
      invalidNodes.remove(targetID);
    }

    // convert into valid one
    while (!invalidNodes.isEmpty()) {
      nodeID = invalidNodes.get(0);
      value = Integer.toString(random.nextInt(0, 10));
      List<List<String>> addable = new ArrayList<>();
      List<List<String>> removable = new ArrayList<>();
      for (List<String> nodeList : allNodes) {
        if (nodeList.contains("node_id(" + nodeID + ")")) {
          removable.add(nodeList);
          addable.add(Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + 0 + ")", "type(" + nodeID + ",variable)", "value(" + nodeID + "," + value + ")"));
        }
      }
      for (List<String> removableElement : removable) {
        allNodes.remove(removableElement);
      }
      allNodes.addAll(addable);
      invalidNodes.remove(nodeID);
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

  public static void resetProlog(List<String> factsNames) {
    for (String fact : factsNames) {
      Query.hasSolution("abolish(" + fact + ").");
    }
  }

  private static List<LinkedHashMap<String, Object>> analyseTreeGeneration(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();

    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      resetProlog(factsNames);
      List<String> operatorValues = Arrays.asList("+", "/", "*", "-");
      graph = generateBinaryTreeGraph(dimension, domainDefinition, operatorValues);

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

  private static void exportTreeAnalysis(List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    //// Analysis:
    int nGraphs = 25;
    int nOperations = 40;

    int dimension = 10;
    List<LinkedHashMap<String, Object>> DataFrame10 = analyseTreeGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 25;
    List<LinkedHashMap<String, Object>> DataFrame25 = analyseTreeGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 40;
    List<LinkedHashMap<String, Object>> DataFrame40 = analyseTreeGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 55;
    List<LinkedHashMap<String, Object>> DataFrame55 = analyseTreeGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    String[] files = {"Dataframe10.csv", "Dataframe25.csv", "Dataframe40.csv", "Dataframe55.csv"};
    List<List<LinkedHashMap<String, Object>>> dfCollection = new ArrayList<>();
    dfCollection.add(DataFrame10);
    dfCollection.add(DataFrame25);
    dfCollection.add(DataFrame40);
    dfCollection.add(DataFrame55);


    //// Export CSV
    for (int i = 0; i < dfCollection.size(); ++i) {
      String fileName = "Tree" + files[i];
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
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");

    final List<String> structuralRules = Arrays.asList(
            "variable_val(X) :- integer(X), X>= 0, X < 10.",
            "operator_val(+).",
            "operator_val(*).",
            "operator_val(-).",
            "operator_val(/).",
            "start_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 0.",
            "node_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 1.",
            "operator_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 2.",
            "variable_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 0.",
            "check_start :- findall(N,start(N,1), N),length(N,N1), N1 == 1.",
            "start_connected(N) :- start(N,1).",
            "start_connected(N) :- edge(N,X,_), start_connected(X).",
            "is_valid :- check_start," +
                    "    foreach(findall(N,node_id(N),Node),maplist(start_connected,Node))," +
                    "    foreach(findall(T,(node_id(T),start(T,1)),T), maplist(start_outdegree,T))," +
                    "    foreach(findall(T,(node_id(T),start(T,0)),T), maplist(node_outdegree,T))," +
                    "    foreach(findall(O,type(O,operator),O), maplist(operator_indegree,O))," +
                    "    foreach(findall(V,type(V,variable),V), maplist(variable_indegree,V)).");

    final List<String> factsNames = Arrays.asList("node_id/1",
            "start/2",
            "type/2",
            "value/2",
            "edge_id/1",
            "edge/3");

    //// Operators
    List<String> operators = new ArrayList<>();
    List<String> operatorsLabels = new ArrayList<>();

    String subTree = "findall(VV,type(VV,variable),VAR)," +
            "random_member(V,VAR)," +
            "retract(value(V,_))," +
            "retract(type(V,variable))," +
            "operator_val(OpVal)," +
            "assert(type(V,operator))," +
            "assert(value(V,OpVal))," +
            "gensym(nod,N1)," +
            "assert(node_id(N1))," +
            "assert(type(N1,variable))," +
            "random_between(0,9,V1Val)," +
            "assert(value(N1,V1Val))," +
            "assert(start(N1,0))," +
            "gensym(nod,N2)," +
            "assert(node_id(N2))," +
            "assert(type(N2,variable))," +
            "random_between(0,9,V2Val)," +
            "assert(value(N2,V2Val))," +
            "assert(start(N2,0))," +
            "gensym(edge,E1)," +
            "gensym(edge,E2)," +
            "assert(edge_id(E1))," +
            "assert(edge_id(E2))," +
            "assert(edge(N1,V,E1))," +
            "assert(edge(N2,V,E2)).";
    operators.add(subTree);
    operatorsLabels.add("subTree");

    String perturbOperator = "findall(OP,type(OP,operator), Operators)," +
            "random_member(O, Operators)," +
            "retract(value(O,_))," +
            "findall(V,operator_val(V),Values)," +
            "random_member(X,Values)," +
            "assert(value(O,X))";
    operators.add(perturbOperator);
    operatorsLabels.add("perturbOperator");

    String perturbVariable = "findall(VAR,type(VAR,variable), Variables)," +
            "random_member(O, Variables)," +
            "retract(value(O,_))," +
            "random_between(0,9,X)," +
            "assert(value(O,X))";
    operators.add(perturbVariable);
    operatorsLabels.add("perturbVariable");


//    ////Export CSV
    exportTreeAnalysis(operators, operatorsLabels, factsNames, domainDefinition, structuralRules);


  }
}
