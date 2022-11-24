package it.units.malelab.jgea.core.representation.graph.prolog.analysis;

import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;
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

  @Deprecated
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

  @Deprecated
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

  @Deprecated
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
            "operator_val(+).",
            "operator_val(*).",
            "operator_val(-).",
            "operator_val(/).",
            "n_input(3).",
            "input_val(X) :- n_input(Max), integer(X), X>=0, X<Max.",
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

    final List<String> factsNames = Arrays.asList("node_id/1",
            "start/2",
            "type/2",
            "value/2",
            "edge_id/1",
            "edge/3");

    //// Operators
    List<String> operators = new ArrayList<>();
    List<String> operatorsLabels = new ArrayList<>();

    String subTree = "findall(VV,(type(VV,input); type(VV,constant)),VAR),\n" +
            "random_member(V,VAR),\n" +
            "retract(value(V,_)),\n" +
            "retract(type(V,_)),\n" +
            "operator_val(OpVal),\n" +
            "assert(type(V,operator)),\n" +
            "assert(value(V,OpVal)),\n" +
            "gensym(nod,N1),\n" +
            "assert(node_id(N1)),\n" +
            "assert(start(N1,0)),\n" +
            "n_input(NInp),\n" +
            "InpMax is NInp -1,\n" +
            "(   maybe ->  assert(type(N1,input)),\n" +
            "                     random(0, InpMax, InpVal),\n" +
            "                     assert(value(N1,InpVal)); \n" +
            "    assert(type(N1,constant)),\n" +
            "                     random(0,10,V1Val),\n" +
            "                     assert(value(N1,V1Val)) ),\n" +
            "gensym(nod,N2),\n" +
            "assert(node_id(N2)),\n" +
            "assert(start(N2,0)),\n" +
            "(   maybe ->  assert(type(N2,input)),\n" +
            "                     random(0, InpMax, InpVal2),\n" +
            "                     assert(value(N2,InpVal2)); \n" +
            "    assert(type(N2,constant)),\n" +
            "                     random(0,10,V2Val),\n" +
            "                     assert(value(N2,V2Val)) ),\n" +
            "gensym(edge,E1),\n" +
            "gensym(edge,E2),\n" +
            "assert(edge_id(E1)),\n" +
            "assert(edge_id(E2)),\n" +
            "assert(edge(N1,V,E1)),\n" +
            "assert(edge(N2,V,E2)).";
    operators.add(subTree);
    operatorsLabels.add("subTree");

    String changeOperator = "findall(OP,type(OP,operator), Operators)," +
            "random_member(O, Operators)," +
            "retract(value(O,_))," +
            "findall(V,operator_val(V),Values)," +
            "random_member(X,Values)," +
            "assert(value(O,X))";
    operators.add(changeOperator);
    operatorsLabels.add("changeOperator");

    String changeConstant = "findall(CON,type(CON,constant), Constants)," +
            "random_member(O, Constants)," +
            "retract(value(O,_))," +
            "random(0,9,X)," +
            "assert(value(O,X)).";
    operators.add(changeConstant);
    operatorsLabels.add("changeConstant");

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
            "n_input(NInp)," +
            "InpMax is NInp -1," +
            "(maybe -> assert(type(T,variable))," +
            "   random(0,InpMax,InpVal)," +
            "   assert(value(N1,InpVal));" +
            "assert(type(T,input))," +
            "   random(0,10,V1Val)," +
            "   assert(value(N1,V1Val)) ).";
    operators.add(removeLeaves);
    operatorsLabels.add("removeLeaves");

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
    operatorsLabels.add("swapEdges");


//    ////Export CSV
//    exportTreeAnalysis(operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

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
    node3.put("value", 0);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "firstEdge");
    LinkedHashMap<String, Object> edge2 = new LinkedHashMap<>();
    edge2.put("edge_id", "secondEdge");
    origin.addNode(node1);
    origin.addNode(node2);
    origin.addNode(node3);
    origin.setArcValue(node2, node1, edge1);
    origin.setArcValue(node3, node1, edge2);

    String name = "ZZNEWTreeSelection";
    List<String> factoryOperators = Arrays.asList(subTree, changeOperator);

    PrologGraphFactory.exportFactoryAnalysis(name, 25, 49, origin, factoryOperators, domainDefinition, structuralRules);
    PrologGraphFactory.exportFactoryAnalysis(name, 50, 74, origin, factoryOperators, domainDefinition, structuralRules);
    PrologGraphFactory.exportFactoryAnalysis(name, 75, 99, origin, factoryOperators, domainDefinition, structuralRules);


  }
}
