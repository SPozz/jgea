package it.units.malelab.jgea.core.representation.graph.prolog;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Analysis {
  public static void main(String[] args) {

    // Trees' structuralRules
    List<String> treeRules;
    try (Stream<String> treeRulesPath = Files.lines(Paths.get("./prolog/trees/structuralRules.txt"))) {
      treeRules = treeRulesPath.collect(Collectors.toList());
      treeRules.add(0, "max_const(10.0).");
      treeRules.add(0, "min_const(0.1).");
      treeRules.add(0, "n_input(1).");
    } catch (IOException e) {
      throw new UnsupportedOperationException("Tree's structural rules not found in given path");
    }

    // Ffnns' structuralRules
    List<String> ffnnRules;
    int ffnnInput = 2;
    try (Stream<String> ffnnRulesPath = Files.lines(Paths.get("./prolog/ffnn/structuralRules.txt"))) {
      ffnnRules = ffnnRulesPath.collect(Collectors.toList());
      ffnnRules.add(0, "max_weight(5.0).");
      ffnnRules.add(0, "min_weight(-5.0).");
      ffnnRules.add(0, "n_output(1).");
      ffnnRules.add(0, "max_size(141).");
      ffnnRules.add(0, "n_input(" + ffnnInput + ").");

    } catch (IOException e) {
      throw new UnsupportedOperationException("Ffnn's structural rules not found in given path");
    }

    // DFAs' structuralRules
    List<String> dfaRules;
    try (Stream<String> dfaRulesPath = Files.lines(Paths.get("./prolog/fsm/structuralRules.txt"))) {
      dfaRules = dfaRulesPath.collect(Collectors.toList());
    } catch (IOException e) {
      throw new UnsupportedOperationException("Fsm structural rules not found in given path");
    }
    int nSymbols = 2;
    for (int i = 0; i < nSymbols; ++i) {
      dfaRules.add(0, "input_val(" + i + ").");
    }

    // Tree
    final PrologGraph treeOrigin = getTreeOrigin();
    final List<String> treeDomain = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");
    final String treeOperatorsPath = "./prolog/trees/operators/";
    List<List<String>> treePrologOperators = getLabelledOperators(treeOperatorsPath + "others");
    treePrologOperators.addAll(getLabelledOperators(treeOperatorsPath + "selection"));

    // Ffnn
    final PrologGraph ffnnOrigin = getFfnnOrigin(ffnnInput);
    final List<String> ffnnDomain = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic layer/2.",
            ":- dynamic bias/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic weight/2.");
    final String ffnnOperatorsPath = "./prolog/ffnn/operators/";
    List<List<String>> ffnnPrologOperators = getLabelledOperators(ffnnOperatorsPath + "others");
    ffnnPrologOperators.addAll(getLabelledOperators(ffnnOperatorsPath + "selection"));

    // Dfa
    final PrologGraph dfaOrigin = getDFAOrigin("[0,1]");
    final List<String> dfaDomain = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");
    final String dfaOperatorsPath = "./prolog/fsm/operators/";
    List<List<String>> dfaPrologOperators = getLabelledOperators(dfaOperatorsPath + "others");
    dfaPrologOperators.addAll(getLabelledOperators(dfaOperatorsPath + "selection"));


    final int nGraphs = 50;
    final int nOperations = 25;
    AnalysisUtils.exportAnalysis("Tree50x25.csv", 5, 105, nGraphs, nOperations, treeOrigin, treePrologOperators, treeDomain, treeRules);
    AnalysisUtils.exportAnalysis("Ffnn50x25.csv", 5, 105, nGraphs, nOperations, ffnnOrigin, ffnnPrologOperators, ffnnDomain, ffnnRules);
    AnalysisUtils.exportAnalysis("Dfa50x25.csv", 2, 102, nGraphs, nOperations, dfaOrigin, dfaPrologOperators, dfaDomain, dfaRules);


    // BASIC
    final List<String> basicDomainDefinition = Arrays.asList(":- dynamic node_id/1.", ":- dynamic attribute/2.", ":- dynamic edge_id/1.", ":- dynamic edge/3.", ":- dynamic colour/2.");
    final List<String> basicStructuralRules = Arrays.asList("is_valid :- true.",
            "retract_list([X | Xs], P) :- " +
                    "        Z =.. [P, X], retract(Z), retract_list(Xs, P).",
            "retract_list([], _) :- true.",
            "retract_list([X|Xs],P,S) :- Z=.. [P,X,S], retract(Z), retract_list(Xs,P,S).",
            "retract_list([],_,S) :- true.",
            "attribute_value(X) :- float(X), X =< 1, X >= 0.",
            "colour_value(red).",
            "colour_value(blue).",
            "colour_value(green).",
            "colour_value(orange).",
            "colour_value(yellow)."
    );

    // Operators' definition
    List<String> addNode = Arrays.asList("AddNode", "gensym(nod,X), assert(node_id(X)), random(0.0,1.0,V), attribute_value(V), assert(attribute(X,V)).");
    List<String> addEdge = Arrays.asList("addEdge", "findall(N,node_id(N), Nodes)," +
            "random_member(Source, Nodes)," +
            "random_member(Target, Nodes), " +
            "gensym(edg,E)," +
            "assert(edge_id(E))," +
            "assert(edge(Source,Target,E))," +
            "findall(C,colour_value(C),CDomain)," +
            "random_member(RED,CDomain)," +
            "assert(colour(E,RED)).");
    List<String> removeEdge = Arrays.asList("removeEdge", "findall(EID,edge_id(EID),Ids)," +
            "random_member(Removable,Ids)," +
            "retract(edge_id(Removable))," +
            "retract(colour(Removable,_))," +
            "retract(edge(_,_,Removable)).");

    List<String> intermediateNode = Arrays.asList("intermediateNode", "findall((S,T,I),edge(S,T,I),Edges)," +
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
            "random(0.0,1.0,V), assert(attribute(X,V))," +
            "assert(edge(N1,N,E1))," +
            "assert(edge(N,N2,E2)).");
    List<String> perturbNode25 = Arrays.asList("perturbNode25", "findall(X,node_id(X),Nodes)," +
            "random_member(N,Nodes)," +
            "attribute(N,V)," +
            "random(0.0,0.25,R)," +
            "New_Val is R*V," +
            "(attribute_value(New_Val) ->  retract(attribute(N,V))," +
            "                    assertz(attribute(N,New_Val));" +
            "    true).");
    List<String> changeColor = Arrays.asList("changeColor", "findall(ID,edge_id(ID),IDs)," +
            "random_member(E,IDs)," +
            "findall(C,colour_value(C),Colours)," +
            "random_member(V,Colours)," +
            "retract(colour(E,_))," +
            "assert(colour(E,V)).");
    List<String> removeNode = Arrays.asList("removeNode", "findall(NId,node_id(NId),Nodes)," +
            "random_member(N,Nodes)," +
            "findall(E,edge(_,N,E),ID_in)," +
            "findall(E,edge(N,_,E),ID_out)," +
            "retract_list(ID_in,colour,_)," +
            "retract_list(ID_out,colour,_)," +
            "retract_list(ID_in,edge_id)," +
            "retract_list(ID_out,edge_id)," +
            "retractall(edge(_,N,_))," +
            "retractall(edge(N,_,_))," +
            "retract(attribute(N,_))," +
            "retract(node_id(N)).");

    List<List<String>> basicOperators = Arrays.asList(removeNode, perturbNode25, intermediateNode, addNode, addEdge, removeEdge, changeColor);

    PrologGraph basicOrigin = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("attribute", 0.5d);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "firstEdge");
    edge1.put("colour", "red");
    basicOrigin.addNode(node1);
    basicOrigin.setArcValue(node1, node1, edge1);


    AnalysisUtils.exportAnalysis("Basic50x25.csv", 5, 105, nGraphs, nOperations, basicOrigin, basicOperators, basicDomainDefinition, basicStructuralRules);


  }

  private static PrologGraph getDFAOrigin(String inputLoop) {
    PrologGraph fsm = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("start", 1);
    node1.put("accepting", 1);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "loopEdge");
    edge1.put("input", inputLoop);
    fsm.addNode(node1);
    fsm.setArcValue(node1, node1, edge1);
    return fsm;
  }

  private static PrologGraph getTreeOrigin() {
    PrologGraph tree = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("start", 1);
    node1.put("type", "operator");
    node1.put("value", "+");
    LinkedHashMap<String, Object> node2 = new LinkedHashMap<>();
    node2.put("node_id", "second");
    node2.put("start", 0);
    node2.put("type", "constant");
    node2.put("value", 1d);
    LinkedHashMap<String, Object> node3 = new LinkedHashMap<>();
    node3.put("node_id", "third");
    node3.put("start", 0);
    node3.put("type", "input");
    node3.put("value", 0);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "firstEdge");
    LinkedHashMap<String, Object> edge2 = new LinkedHashMap<>();
    edge2.put("edge_id", "secondEdge");
    tree.addNode(node1);
    tree.addNode(node2);
    tree.addNode(node3);
    tree.setArcValue(node2, node1, edge1);
    tree.setArcValue(node3, node1, edge2);
    return tree;
  }

  private static PrologGraph getFfnnOrigin(int nInput) {
    PrologGraph ffnn = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("layer", 0);
    node1.put("bias", 0d);
    LinkedHashMap<String, Object> node2 = new LinkedHashMap<>();
    node2.put("node_id", "second");
    node2.put("layer", 1);
    node2.put("bias", 1.0d);
    LinkedHashMap<String, Object> node3 = new LinkedHashMap<>();
    node3.put("node_id", "third");
    node3.put("layer", 2);
    node3.put("bias", 0d);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "firstEdge");
    edge1.put("weight", 0.5d);
    LinkedHashMap<String, Object> edge2 = new LinkedHashMap<>();
    edge2.put("edge_id", "secondEdge");
    edge2.put("weight", 0.2d);
    ffnn.addNode(node1);
    ffnn.addNode(node2);
    ffnn.addNode(node3);
    ffnn.setArcValue(node1, node2, edge1);
    ffnn.setArcValue(node2, node3, edge2);
    for (int i = 1; i < nInput; ++i) {
      LinkedHashMap<String, Object> node = new LinkedHashMap<>();
      node.put("node_id", "first" + i);
      node.put("layer", 0);
      node.put("bias", 0);
      ffnn.addNode(node);

      LinkedHashMap<String, Object> edge = new LinkedHashMap<>();
      edge.put("edge_id", "edge" + i);
      edge.put("weight", 0.5d);
      ffnn.setArcValue(node, node2, edge);

    }
    return ffnn;
  }

  private static List<List<String>> getLabelledOperators(String pathToFolder) {
    List<List<String>> labelledOperators = new ArrayList<>();
    try {
      final File folder = new File(pathToFolder);
      File[] files = folder.listFiles();
      if (files == null) {
        throw new UnsupportedOperationException("No files defined in " + pathToFolder);
      } else {
        for (File file : files) {
          String operator = Files.readString(file.toPath());
          labelledOperators.add(Arrays.asList(file.getName().replace(".txt", ""), operator));
        }
      }
    } catch (IOException ioException) {
      throw new UnsupportedOperationException("IOException in trees' factories.");
    }
    return labelledOperators;
  }


}
