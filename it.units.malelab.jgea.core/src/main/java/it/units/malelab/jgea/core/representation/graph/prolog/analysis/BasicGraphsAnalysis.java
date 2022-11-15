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

public class BasicGraphsAnalysis {

  private static PrologGraph generateBasicGraph(int dimension, List<String> domainDefinition) {
    Random random = new Random();

    final List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    final List<String> colours = Arrays.asList("red", "blue", "yellow", "orange", "green");
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> nodesIDS = new ArrayList<>();

    // Attributes for graph
    final int nNodeAttributes = 2;  //ID, attribute
    final int nArcAttributes = 3;   //ID, edge, colour
    if ((nArcAttributes + nNodeAttributes) != domainDefinition.size()) {
      throw new UnsupportedOperationException("Wrong definition of number of attributes");
    }

    List<String> indexList = new ArrayList<>();
    int debuggerID = 1;

    final int nNodes = random.nextInt(dimension / 2, dimension - 1);

    for (int i = 0; i < nNodes; ++i) {
      final int index = random.nextInt(0, alphabet.size());
      String nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debuggerID;
        debuggerID++;
      }
      indexList.add(Integer.toString(index));

      final double value = random.nextDouble(0, 1);
      nodesIDS.add(nodeID);
      allNodes.add(Arrays.asList("node_id(" + nodeID + ")", "attribute(" + nodeID + "," + value + ")"));
    }

    List<String> edgeIDs = new ArrayList<>();
    int recursion = 1;
    final int maxRecursion = 100;
    for (int j = 0; j < (dimension - nNodes); ++j) {
      if (recursion >= maxRecursion) // limit max recursion
        break;
      String source = nodesIDS.get(random.nextInt(0, nodesIDS.size()));
      String target = nodesIDS.get(random.nextInt(0, nodesIDS.size()));
      String edgeID = source + target;
      if (edgeIDs.contains(edgeID)) { // if edge already exists, re-do
        --j;
        recursion++;
        continue;
      }
      edgeIDs.add(edgeID);
      recursion = 1; // reset if not infinite loop
      String colour = colours.get(random.nextInt(0, colours.size()));
      allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "colour(" + edgeID + "," + colour + ")"));
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

  private static List<LinkedHashMap<String, Object>> analysis(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();

    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      resetProlog(factsNames);
      graph = generateBasicGraph(dimension, domainDefinition);

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
    // Subset of the graph:
    final List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.", ":- dynamic attribute/2.", ":- dynamic edge_id/1.", ":- dynamic edge/3.", ":- dynamic colour/2.");

    final List<String> factsNames = Arrays.asList("node_id/1", "attribute/2", "edge_id/1", "edge/3", "colour/2");

    final List<String> structuralRules = Arrays.asList("is_valid :- true.",
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
    final String operatorAddNodeWithAttribute = "gensym(nod,X), assert(node_id(X)), random(V), attribute_value(V), assert(attribute(X,V)).";

    final String operatorAddEdgeWithAttribute = "findall(N,node_id(N), Nodes)," +
            "random_member(Source, Nodes)," +
            "random_member(Target, Nodes), " +
            "gensym(edg,E)," +
            "assert(edge_id(E))," +
            "assert(edge(Source,Target,E))," +
            "findall(C,colour_value(C),CDomain)," +
            "random_member(RED,CDomain)," +
            "assert(colour(E,RED)).";

    final String operatorRemoveEdgeWithAttribute = "findall(EID,edge_id(EID),Ids)," +
            "random_member(Removable,Ids)," +
            "retract(edge_id(Removable))," +
            "retract(colour(Removable,_))," +
            "retract(edge(_,_,Removable)).";

    final String operatorIntermediateNodeWithAttributes = "findall((S,T,I),edge(S,T,I),Edges)," +
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
            "random(V), attribute_value(V), assert(attribute(X,V))," + //if with attribute
            "assert(edge(N1,N,E1))," +
            "assert(edge(N,N2,E2)).";

    final String operatorPerturbValue = "findall(X,node_id(X),Nodes)," +
            "random_member(N,Nodes)," +
            "attribute(N,V)," +
            "random(R)," +
            "New_Val is R*V," +
            "(attribute_value(New_Val) ->  retract(attribute(N,V))," +
            "                    assertz(attribute(N,New_Val));" +
            "    true).";

    final String operatorModifyEdgeValue = "findall(ID,edge_id(ID),IDs)," +
            "random_member(E,IDs)," +
            "findall(C,colour_value(C),Colours)," +
            "random_member(V,Colours)," +
            "retract(colour(E,_))," +
            "assert(colour(E,V)).";

    final String operatorRemoveNode = "findall(NId,node_id(NId),Nodes)," +
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
            "retract(node_id(N)).";


    final List<String> operators = Arrays.asList(operatorRemoveNode, operatorAddEdgeWithAttribute, operatorAddNodeWithAttribute, operatorModifyEdgeValue, operatorPerturbValue, operatorIntermediateNodeWithAttributes, operatorRemoveEdgeWithAttribute);
    final List<String> operatorsLabels = Arrays.asList("removeNode", "addEdge", "addNode", "modifyEdgeValue", "perturbNodeValue", "intermediateNode", "removeEdge");


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


    ////EXPORT CSV
    for (int i = 0; i < dfCollection.size(); ++i) {
      String fileName = "Basic" + files[i];
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

