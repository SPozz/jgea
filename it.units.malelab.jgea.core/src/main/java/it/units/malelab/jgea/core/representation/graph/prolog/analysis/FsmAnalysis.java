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

public class FsmAnalysis {

  public static PrologGraph generateFSMGraph(int dimension, List<String> domainDefinition, List<Character> inputSymbols) {
    Random random = new Random();
    List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> nodesIDS = new ArrayList<>();

    // Attributes for graph
    final int nNodeAttributes = 3; // including ID
    final int nArcAttributes = 3; // including id and edge
    if ((nArcAttributes + nNodeAttributes) != domainDefinition.size()) {
      throw new UnsupportedOperationException("Wrong definition of number of attributes");
    }

    List<String> indexList = new ArrayList<>();
    int debuggerID = 1;

    final int nNodes = dimension / (inputSymbols.size() + 1);

    for (int i = 0; i < nNodes; ++i) {
      int index = random.nextInt(0, alphabet.size());
      String nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debuggerID;
        debuggerID++;
      }
      indexList.add(Integer.toString(index));
      nodesIDS.add(nodeID);

      int start = 0;
      if (i == 0) {
        start = 1;
      }
      final int accepting = random.nextInt(0, 2);

      allNodes.add(Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "accepting(" + nodeID + "," + accepting + ")"));
    }

    Set<String> edgeIDs = new HashSet<>();
    for (String source : nodesIDS) {
      for (Character input : inputSymbols) { //add as many edges as input symbols
        String target = nodesIDS.get(random.nextInt(0, nodesIDS.size()));

        String edgeID = source + target;
        String inputSequence = "" + input;

        if (edgeIDs.contains(edgeID)) { //if edge already exists, create unique one with multiple symbol separated by "," as [0,1,2]
          for (List<String> edge : allEdges) {
            if (edge.contains("edge_id(" + edgeID + ")")) {
              final int removableEdgeIndex = allEdges.indexOf(edge);
              final int edgeIdLength = edge.get(0).length() - "edge_id()".length();
              String previousCharacters = edge.get(2).substring("input(,[".length() + edgeIdLength);
              previousCharacters = previousCharacters.replace(")", "");
              previousCharacters = previousCharacters.replace("]", "");
              allEdges.remove(removableEdgeIndex);
              inputSequence += ("," + previousCharacters);
              break;
            }
          }
        }
        edgeIDs.add(edgeID);
        allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "input(" + edgeID + ",[" + inputSequence + "])"));
      }
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

  private static List<LinkedHashMap<String, Object>> analyseFsmGeneration(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();

    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      BasicGraphsAnalysis.resetProlog(factsNames);
      List<Character> inputSymbols = Arrays.asList('0', '1'); //Here we defined operator for this case
      graph = generateFSMGraph(dimension, domainDefinition, inputSymbols);

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

  private static void exportFsmAnalysis(List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    // Analysis:
    int nGraphs = 25;
    int nOperations = 40;

    int dimension = 9;
    List<LinkedHashMap<String, Object>> DataFrame10 = analyseFsmGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 24;
    List<LinkedHashMap<String, Object>> DataFrame25 = analyseFsmGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 39;
    List<LinkedHashMap<String, Object>> DataFrame40 = analyseFsmGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 54;
    List<LinkedHashMap<String, Object>> DataFrame55 = analyseFsmGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    String[] files = {"Dataframe10.csv", "Dataframe25.csv", "Dataframe40.csv", "Dataframe55.csv"};
    List<List<LinkedHashMap<String, Object>>> dfCollection = new ArrayList<>();
    dfCollection.add(DataFrame10);
    dfCollection.add(DataFrame25);
    dfCollection.add(DataFrame40);
    dfCollection.add(DataFrame55);

    // Export CSV
    for (int i = 0; i < dfCollection.size(); ++i) {
      String fileName = "FSM" + files[i];
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
    // Subset definition:
    final List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");

    final List<String> factsNames = Arrays.asList("node_id/1", "start/2", "accepting/2", "edge_id/1", "edge/3", "input/2");

    final List<String> structuralRules = Arrays.asList(
            "input_val(1).",
            "input_val(0).",
            "n_input(N) :- findall(X,input_val(X),Domain), length(Domain,N).",
            "accepting_domain(X) :- integer(X), X =< 1, X >= 0.",
            "start_domain(X) :- integer(X), X =< 1, X >= 0.",
            "check_start :- findall(N,start(N,1), N), length(N,N1), N1 == 1.",
            "check_accepting :- findall(N,accepting(N,1), N), length(N,N1), N1 >= 1.",
            "symbols(S) :- findall(X,(edge(S,_,ID),input(ID,X)),Inputs), flatten(Inputs,List), list_to_set(List,Set), List == Set.",
            "outdegree(S,LEN) :- findall(X,(edge(S,_,ID),input(ID,X)),Inputs), flatten(Inputs,K), length(K,LEN).",
            "random_pair(Z1,Z2,List) :-" +
                    "    random_member(Z1,List)," +
                    "    random_member(Z2,List)," +
                    "    Z1 \\== Z2.",
            "retract_list([], _) :- true.",
            "retract_list([X | Xs], P) :- " +
                    "        Z =.. [P, X], retract(Z), retract_list(Xs, P).",
            "retract_list([X|Xs],P,S) :- Z=.. [P,X,S], retract(Z), retract_list(Xs,P,S).",
            "retract_list([],_,_) :- true.",
            "is_valid :- check_start, " +
                    "    foreach(findall(N,node_id(N),Nodes),maplist(symbols,Nodes))," +
                    "    check_accepting."
    );

    // Operators:
    List<String> operators = new ArrayList<>();
    List<String> operatorsLabels = new ArrayList<>();

    String addNode = "gensym(nod,N)," +
            "assert(node_id(N))," +
            "random_between(0,1,INT)," +
            "assert(accepting(N,INT))," +
            "assert(start(N,0)).";
    operatorsLabels.add("addNode");
    operators.add(addNode);

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
            "assert(accepting(M,V)).";
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

    String addNodeAndEdges = "findall(N,node_id(N),Nodes)," +
            "findall(V,input_val(V),Values)," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "random_between(0,1,INT)," +
            "assert(accepting(N,INT))," +
            "assert(start(N,0)), " +
            "random_member(X,Nodes)," +
            "random_member(Y,Nodes)," +
            "random_pair(V1,V2,Values)," +
            "gensym(edg,Xedge)," +
            "assert(edge_id(Xedge))," +
            "assert(edge(N,X,Xedge))," +
            "(   X == Y ->  " +
            "    assert(input(Xedge,[V1,V2]))" +
            "    ;" +
            "    assert(input(Xedge,[V1]))," +
            "    gensym(edg,Yedge)," +
            "assert(edge_id(Yedge))," +
            "assert(edge(N,Y,Yedge))," +
            "    assert(input(Yedge,[V2]))" +
            "    ).";
    operatorsLabels.add("addNodeAndEdges");
    operators.add(addNodeAndEdges);

    String addConnectedNode = "findall(N,node_id(N),Nodes)," +
            "findall(V,input_val(V),Values)," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "random_between(0,1,INT)," +
            "assert(accepting(N,INT))," +
            "assert(start(N,0)), " +
            "findall(E,edge_id(E),EdgeIDs)," +
            "random_member(F,EdgeIDs)," +
            "edge(S,_,F)," +
            "retract(edge(S,_,F))," +
            "assert(edge(S,N,F))," +
            "random_member(X,Nodes)," +
            "random_member(Y,Nodes)," +
            "random_pair(V1,V2,Values)," +
            "gensym(edg,Xedge)," +
            "assert(edge_id(Xedge))," +
            "assert(edge(N,X,Xedge))," +
            "(X == Y ->  " +
            "assert(input(Xedge,[V1,V2]));" +
            "assert(input(Xedge,[V1]))," +
            "gensym(edg,Yedge)," +
            "assert(edge_id(Yedge))," +
            "assert(edge(N,Y,Yedge))," +
            "assert(input(Yedge,[V2]))" +
            ").";
    operatorsLabels.add("addConnectedNode");
    operators.add(addConnectedNode);

    String addMissingTransition = "n_input(Ninp), findall(N,(node_id(N),outdegree(N,M), M < Ninp), Nodes), " +
            "random_member(Nod,Nodes)," +
            "findall(V,input_val(V),Values)," +
            "findall(W,(edge(Nod,_,X),input(X,W)),ActualValues)," +
            "flatten(ActualValues,FlattenValues)," +
            "subtract(Values,FlattenValues,Difference)," +
            "length(Difference,D)," +
            "(D == 0 ->   false; " +
            "    random_member(Target,Nodes)," +
            "    random_member(NewValue,Difference)," +
            "    (edge(Nod,Target,ID) ->  " +
            "    input(ID, Inputs)," +
            "        retract(input(ID,Inputs))," +
            "    append([NewValue],Inputs, NewInputs)," +
            "        assert(input(ID,NewInputs));" +
            "    gensym(edg,NewEdge)," +
            "        assert(edge_id(NewEdge))," +
            "        assert(edge(Nod,Target,NewEdge))," +
            "        assert(input(NewEdge,[NewValue])))" +
            "    ).";
    operatorsLabels.add("addMissingTransition");
    operators.add(addMissingTransition);

    String removeNode = "findall(M,node_id(M),NodeIDs)," +
            "random_member(N,NodeIDs)," +
            "findall(E,(edge_id(E),edge(_,N,E)),IdsIncoming)," +
            "retract_list(IdsIncoming,edge_id)," +
            "findall(E,(edge_id(E),edge(N,_,E)),IdsOutcoming)," +
            "retract_list(IdsOutcoming,edge_id)," +
            "retractall(edge(_,N,_))," +
            "retractall(edge(N,_,_))," +
            "retract_list(IdsIncoming,input,_)," +
            "retract_list(IdsOutcoming,input,_)," +
            "retract(start(N,_))," +
            "retract(accepting(N,_))," +
            "retract(node_id(N)).";
    operatorsLabels.add("removeNode");
    operators.add(removeNode);

    String removeTransition = "findall(EE,edge_id(EE),Ids)," +
            "random_member(E,Ids)," +
            "retract(input(E,_))," +
            "retract(edge(_,_,E))," +
            "retract(edge_id(E))";
    operatorsLabels.add("removeTransition");
    operators.add(removeTransition);


//    //Export CSV
    exportFsmAnalysis(operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

//    //Export analysis of Factory
    PrologGraph origin = new PrologGraph();
    LinkedHashMap<String, Object> node = new LinkedHashMap<>();
    node.put("node_id", "start");
    node.put("start", 1);
    node.put("accepting", 1);
    LinkedHashMap<String, Object> edge = new LinkedHashMap<>();
    edge.put("edge_id", "loop");
    edge.put("input", "[0,1]");
    origin.addNode(node);
    origin.setArcValue(node, node, edge);

    String name = "FsmSelection"; //Selection = addConncected, addNode, addNodeAndEdges, addMissingTransition, changeTarget
    List<String> factoryOperators = Arrays.asList(addConnectedNode, addNode, addMissingTransition, addNodeAndEdges, changeTarget);

    PrologGraphFactory.exportFactoryAnalysis(name, 25, 49, origin, factoryOperators, domainDefinition, structuralRules);
    PrologGraphFactory.exportFactoryAnalysis(name, 50, 74, origin, factoryOperators, domainDefinition, structuralRules);
    PrologGraphFactory.exportFactoryAnalysis(name, 75, 99, origin, factoryOperators, domainDefinition, structuralRules);




  }

}
