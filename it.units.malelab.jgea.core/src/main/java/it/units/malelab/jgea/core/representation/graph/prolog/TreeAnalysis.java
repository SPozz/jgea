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

public class TreeAnalysis {


  static PrologGraph generateGraph(int dimension, List<String> domainDefinition) {
    Random random = new Random();

    List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<String> typeValue = Arrays.asList("operator", "variable");
    List<String> operandValue = Arrays.asList("plus", "dot", "minus");

    List<String> node;
    List<String> edge;
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> edgeIDs = new ArrayList<>();

    List<String> indexList = new ArrayList<>();

    int tmpDimension = 1;

    // first node
    int start = 1;
    String type = "operand";
    String value = operandValue.get(random.nextInt(0, operandValue.size()));
    int index = random.nextInt(0, alphabet.size());
    indexList.add(Integer.toString(index));
    String nodeID = alphabet.get(index);
    node = Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")");
    allNodes.add(node);
    tmpDimension += 1;


    List<String> invalidNodes = new ArrayList<>();
    invalidNodes.add(nodeID);

    start = 0;
    int debugger = 1;
    while (!invalidNodes.isEmpty() && tmpDimension < dimension / 2) {
      String sourceID = invalidNodes.get(random.nextInt(0, invalidNodes.size()));

      // first new node
      index = random.nextInt(0, alphabet.size());
      nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debugger;
        debugger++;
      }
      indexList.add(Integer.toString(index));

      type = "operand";
      value = operandValue.get(random.nextInt(0, operandValue.size()));
      node = Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")");

      allNodes.add(node);
      tmpDimension++;
      invalidNodes.add(nodeID);

      // add edge
      String edgeID = sourceID + nodeID;
      if (edgeIDs.contains(edgeID)) {
        edgeID += debugger;
        debugger++;
      }
      edgeIDs.add(edgeID);
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + sourceID + "," + nodeID + "," + edgeID + ")");
      allEdges.add(edge);
      tmpDimension++;


      // second new node
      index = random.nextInt(0, alphabet.size());
      nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debugger;
        debugger++;
      }
      indexList.add(Integer.toString(index));

      type = typeValue.get(random.nextInt(0, typeValue.size()));
      if (type.equals("operand")) {
        invalidNodes.add(nodeID);
        value = operandValue.get(random.nextInt(0, operandValue.size()));
      } else {
        value = Integer.toString(random.nextInt(0, 10));
      }
      node = Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")");

      allNodes.add(node);
      tmpDimension++;

      // add edge
      edgeID = sourceID + nodeID;
      if (edgeIDs.contains(edgeID)) {
        edgeID += debugger;
        debugger++;
      }
      edgeIDs.add(edgeID);
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + sourceID + "," + nodeID + "," + edgeID + ")");
      allEdges.add(edge);
      tmpDimension++;

      invalidNodes.remove(sourceID);
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
    for (int j = 0; j < 4; ++j) {
      for (List<String> oneNode : allNodes) {
        graphDescription.add(oneNode.get(j));
      }
    }

    for (int j = 0; j < 2; ++j) {
      for (List<String> oneEdge : allEdges) {
        graphDescription.add(oneEdge.get(j));
      }
    }

    for (String fact : graphDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    return PrologGraphUtils.buildGraph(domainDefinition);
  }

  static public void resetProlog(List<String> factsNames) {
    for (String fact : factsNames) {
      Query.hasSolution("abolish(" + fact + ").");
    }
  }

  static List<LinkedHashMap<String, Object>> analysis(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();

    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      resetProlog(factsNames);
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
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");

    List<String> structuralRules = Arrays.asList(
            "variable_val(X) :- integer(X), X>= 0, X < 10.",
            "operator_val(plus).",
            "operator_val(dot).",
            "operator_val(minus).",
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

    List<String> factsNames = Arrays.asList("node_id/1",
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
            "variable_val(V1Val)," +
            "assert(value(N1,V1Val))," +
            "assert(start(N1,0))," +
            "gensym(nod,N2)," +
            "assert(node_id(N2))," +
            "assert(type(N2,variable))," +
            "variable_val(V2Val)," +
            "assert(value(N2,V2Val))," +
            "assert(start(N2,0))," +
            "gensym(edge,E1)," +
            "gensym(edge,E2)," +
            "assert(edge_id(E1))," +
            "assert(edge_id(E2))," +
            "assert(edge(V,N1,E1))," +
            "assert(edge(V,N2,E2)).";
    operators.add(subTree);
    operatorsLabels.add("subTree");

//    String swapEdges = "findall(Id,edge_id(Id),Edges)," +
//            "random_member(E1,Edges)," +
//            "random_member(E2,Edges)," +
//            "(E1 == E2 ; " +
//            "    edge(S1,T1,E1)," +
//            "    edge(S2,T2,E2)," +
//            "    retract(edge(S1,T1,E1))," +
//            "    retract(edge(S2,T2,E2))," +
//            "    assert(edge(S1,T2,E1))," +
//            "    assert(edge(S2,T1,E2))" +
//            "    ).";
//    operators.add(swapEdges);
//    operatorsLabels.add("swapEdges"); //MMMM

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


    for (String rule : structuralRules) {
      rule = rule.replace(".", "");
      Query.hasSolution("assert((" + rule + "))");
    }

    PrologGraph graph = generateGraph(15,domainDefinition);
    System.out.println(Query.hasSolution("is_valid"));


//    //// Analysis:
//    int nGraphs = 25;
//    int nOperations = 40;
//
//    int dimension = 10;
//    List<LinkedHashMap<String, Object>> DataFrame10 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);
//
//    dimension = 25;
//    List<LinkedHashMap<String, Object>> DataFrame25 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);
//
//    dimension = 40;
//    List<LinkedHashMap<String, Object>> DataFrame40 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);
//
//    dimension = 55;
//    List<LinkedHashMap<String, Object>> DataFrame55 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);
//
//    String[] files = {"Dataframe10.csv", "Dataframe25.csv", "Dataframe40.csv", "Dataframe55.csv"};
//    List<List<LinkedHashMap<String, Object>>> dfCollection = new ArrayList<>();
//    dfCollection.add(DataFrame10);
//    dfCollection.add(DataFrame25);
//    dfCollection.add(DataFrame40);
//    dfCollection.add(DataFrame55);
//
//
//    //// Export CSV
//    for (int i = 0; i < dfCollection.size(); ++i) {
//      String fileName = "Tree" + files[i];
//      List<LinkedHashMap<String, Object>> df = dfCollection.get(i);
//
//      try {
//        // create a writer
//        Writer writer = Files.newBufferedWriter(Paths.get("C:\\Users\\Simone\\Desktop\\GitHub_Tesi\\jgea_data\\25x40\\" + fileName));
//
//        // write CSV file
//        CSVPrinter printer = CSVFormat.DEFAULT.withHeader("graph", "operator", "dimension", "executionTime").print(writer);
//
//        for (LinkedHashMap<String, Object> map : df) {
//          printer.printRecord(map.get("graph"), map.get("operator"), map.get("dimension"), map.get("executionTime"));
//        }
//
//        // flush the stream
//        printer.flush();
//
//        // close the writer
//        writer.close();
//
//      } catch (IOException ex) {
//        ex.printStackTrace();
//      }
//    }


  }
}
