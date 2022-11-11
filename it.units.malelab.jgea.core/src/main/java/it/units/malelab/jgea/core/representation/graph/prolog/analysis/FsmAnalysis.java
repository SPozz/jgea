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

public class FsmAnalysis {

  public static PrologGraph generateFSMGraph(int dimension, List<String> domainDefinition, List<Object> inputSymbols) {
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

    final int nNodes = dimension / 3;

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

    List<String> edgeIDs = new ArrayList<>();
    final int maxRecursion = 250;
    for (String source : nodesIDS) {
      int recursion = 1;
      for (int h = 0; h < 2; ++h) {
        if (recursion == maxRecursion){
          throw new UnsupportedOperationException("maxRecursion  reached in finding NEW edge target");
        }
        String target = nodesIDS.get(random.nextInt(0, nodesIDS.size()));
        String edgeID = source + target;

        if (edgeIDs.contains(edgeID)) {
          h--;
          recursion++;
          continue;
        }
        edgeIDs.add(edgeID);
        recursion =1;

        final int inputIndex = random.nextInt(0, inputSymbols.size());
        Object input = inputSymbols.get(inputIndex);

        allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "input(" + edgeID + "," + input + ")"));
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

  private static List<LinkedHashMap<String, Object>> analysis(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();

    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      BasicGraphsAnalysis.resetProlog(factsNames);
      List<Object> inputSymbols = Arrays.asList(0, 1); //Here we defined operator for this case
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

  public static void main(String[] args) {
    // Subset definition:
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");

    List<String> factsNames = Arrays.asList("node_id/1", "start/2", "accepting/2", "edge_id/1", "edge/3", "input/2");

    List<String> structuralRules = Arrays.asList("n_input(2).",
            "input_domain(X) :- n_input(MAX), integer(X), X =< MAX -1, X >= 0.",
            "accepting_domain(X) :- integer(X), X =< 1, X >= 0.",
            "start_domain(X) :- integer(X), X =< 1, X >= 0.",
//            "size([], 0) :- true.",
//            "size([_|Xs], N) :- size(Xs, N1), plus(N1,1,N).",
            "check_start :- findall(N,start(N,1), N), length(N,N1), N1 == 1.",
            "check_out(S) :- findall(S,edge(S,_,_),RES), length(RES,N), N == 2.",
            "is_valid :- check_start"
                    + ", foreach(findall(N,node_id(N),N), maplist(check_out,N))."
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
            "  random_member(S1,IDs)," +
            "  gensym(edge,E1), " +
            "  assert(edge_id(E1))," +
            "  assert(edge(N,S1,E1))," +
            "  assert(input(E1,0))," +
            "  random_member(S2,IDs)," +
            "  gensym(edge,E2), " +
            "  assert(edge_id(E2))," +
            "  assert(edge(N,S2,E2))," +
            "  assert(input(E2,1)).";
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
    int nGraphs = 25;
    int nOperations = 40;

    int dimension = 9;
    List<LinkedHashMap<String, Object>> DataFrame10 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 24;
    List<LinkedHashMap<String, Object>> DataFrame25 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 39;
    List<LinkedHashMap<String, Object>> DataFrame40 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 54;
    List<LinkedHashMap<String, Object>> DataFrame55 = analysis(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    String[] files = {"Dataframe10.csv", "Dataframe25.csv", "Dataframe40.csv","Dataframe55.csv"};
    List<List<LinkedHashMap<String, Object>>> dfCollection = new ArrayList<>();
    dfCollection.add(DataFrame10);
    dfCollection.add(DataFrame25);
    dfCollection.add(DataFrame40);
    dfCollection.add(DataFrame55);

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

}
