package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.*;


import java.util.*;

public class PrologGraphUtils {


  static List<String> status = new ArrayList<>();

  private final static PrologGraphUtils INSTANCE = new PrologGraphUtils();

  private PrologGraphUtils() {
  }

  public static PrologGraphUtils getInstance() {
    return INSTANCE;
  }


  public synchronized static List<String> describeGraph(PrologGraph graph, List<String> domainDefinition) {
    // Useful repeated variable
    String nodeID = "node_id";
    String edgeID = "edge_id";

    // Extract facts' names from ":- dynamic fact/n"
    List<String> factsNames = extractFactsNames(domainDefinition);

    // IDs for nodes  MUST ALWAYS be defined (edge_id not necessarily?)
    if (!factsNames.contains(nodeID)) {
      throw new UnsupportedOperationException("node_id MUST be always defined."); //TODO: check error handling
    }

    // Split between nodes and arcs, add source-target in place of edge
    // Now should work even if arcs are not in the graph definition
    int breakpoint = factsNames.size();
    List<String> arcsFactsNames = new ArrayList<>();
    if (factsNames.contains(edgeID)) {
      breakpoint = getBreakpointNodesAndArcs(factsNames);

      arcsFactsNames.add("source");
      arcsFactsNames.add("target");
      arcsFactsNames.addAll(factsNames.subList(breakpoint, factsNames.size()));
      arcsFactsNames.remove("edge");
    }
    List<String> nodesFactsNames = factsNames.subList(0, breakpoint);

    // Create output
    List<String> graphDescription = new ArrayList<>();

    // Get set of nodes
    Set<Map<String, Object>> graphNodes = graph.nodes();
    // Create collection of facts relative to nodes, as many Lists as attributes
    List<List<String>> factsCollection = new ArrayList<>();
    for (int i = 0; i < nodesFactsNames.size(); ++i) {
      factsCollection.add(new ArrayList<>());
    }
    // Populate that collection
    for (Map<String, Object> node : graphNodes) {
      String ID = node.get(nodeID).toString();
      factsCollection.get(0).add(nodeID + "(" + ID + ")");

      for (int i = 1; i < nodesFactsNames.size(); ++i) { // i iterating through attributes
        String attribute = nodesFactsNames.get(i);
        String value = node.get(attribute).toString();
        String complete = attribute + "(" + ID + "," + value + ")";
        factsCollection.get(i).add(complete);
      }
    }
    // Add those to the description
    for (int i = 0; i < nodesFactsNames.size(); ++i) {
      graphDescription.addAll(factsCollection.get(i));
    }

    // Get set of arcs
    Set<PrologGraph.Arc<Map<String, Object>>> graphArcs = graph.arcs();
    // Create collection of facts relative to arcs, as many Lists as attributes
    factsCollection = new ArrayList<>();
    for (int i = 0; i < arcsFactsNames.size() - 1; ++i) { //-1 since source and target go together
      factsCollection.add(new ArrayList<>());
    }
    // Populate that collection
    for (PrologGraph.Arc<Map<String, Object>> arc : graphArcs) {
      String ID = graph.getArcValue(arc).get(edgeID).toString();
      String sourceID = arc.getSource().get(nodeID).toString();
      String targetID = arc.getTarget().get(nodeID).toString();

      factsCollection.get(0).add(edgeID + "(" + ID + ")");
      factsCollection.get(1).add("edge(" + sourceID + "," + targetID + "," + ID + ")");

      for (int i = 3; i < arcsFactsNames.size(); ++i) { //0,1,2 source-target-ID
        String attribute = arcsFactsNames.get(i);
        String value = graph.getArcValue(arc).get(attribute).toString();
        String complete = attribute + "(" + ID + "," + value + ")";
        factsCollection.get(i - 1).add(complete);//-1 to be coherent to numb of lists
      }
    }

    for (int i = 0; i < arcsFactsNames.size() - 1; ++i) { //-1 since source and target go together
      graphDescription.addAll(factsCollection.get(i));
    }

    return graphDescription;
  }

  public synchronized static PrologGraph buildGraph(List<String> domainDefinition) {
    // already applied domain and graph
    // Extract facts' names from ":- dynamic fact/n"
    List<String> factsNames = extractFactsNames(domainDefinition);

    // Split between nodes and arcs, add source-target in place of edge
    int breakpoint = factsNames.size();
    List<String> arcsFactsNames = new ArrayList<>();
    if (factsNames.contains("edge_id")) {
      breakpoint = getBreakpointNodesAndArcs(factsNames);

      arcsFactsNames.addAll(factsNames.subList(breakpoint, factsNames.size()));
      arcsFactsNames.remove("edge");
    }
    List<String> nodesFactsNames = factsNames.subList(0, breakpoint);

    // Create output variable
    PrologGraph graph = new PrologGraph();

    // Get (Prolog) a list of node_ids
    Query nodeIDQuery = new Query("node_id(X)");
    if (!nodeIDQuery.hasSolution()) //if no node id => empty graph
      return graph;
    Map<String, Term>[] allNodeIDs = nodeIDQuery.allSolutions();

    // Create list of nodes
    List<LinkedHashMap<String, Object>> nodesCollection = new ArrayList<>();

    // Add nodes to graph
    LinkedHashMap<String, Object> nodeMap;
    for (Map<String, Term> nodeIDMap : allNodeIDs) { // iterating through nodeIDs
      String oneNodeID = nodeIDMap.get("X").toString();
      nodeMap = new LinkedHashMap<>();
      nodeMap.put("node_id", oneNodeID);

      for (String attribute : nodesFactsNames.subList(1, nodesFactsNames.size())) { // iterating through attributes
        String query = attribute + "(" + oneNodeID + ",X)";
        Object value = Query.oneSolution(query).get("X");
        nodeMap.put(attribute, value);
      }
      nodesCollection.add(nodeMap);
      graph.addNode(nodeMap);
    }


    // Get (Prolog) a list of edge_ids
    new Query("assert(edge_id(testing)).").hasSolution(); // otherwise next check crashes
    new Query("retract(edge_id(testing)).").hasSolution();
    Query edgeIDQuery = new Query("edge_id(X)");
    if (!edgeIDQuery.hasSolution()) {
      return graph; // if no edges => we're done
    }
    java.util.Map<String, Term>[] allEdgeIDs = edgeIDQuery.allSolutions();

    // Add edges to graph
    String source;
    String target;
    LinkedHashMap<String, Object> edgeMap;
    for (Map<String, Term> edgeIDMap : allEdgeIDs) { // iterating through nodeIDs
      String oneEdgeID = edgeIDMap.get("X").toString();
      edgeMap = new LinkedHashMap<>();

      edgeMap.put("edge_id", oneEdgeID);

      LinkedHashMap<String, Object> sourceNode = new LinkedHashMap<>();
      LinkedHashMap<String, Object> targetNode = new LinkedHashMap<>();
      source = Query.oneSolution("edge(S,T," + oneEdgeID + ")").get("S").toString();
      target = Query.oneSolution("edge(S,T," + oneEdgeID + ")").get("T").toString();

      for (LinkedHashMap<String, Object> node : nodesCollection) {
        if (node.get("node_id").equals(source)) { //if we accept loops those two "if" should not exclude each other
          sourceNode = node;
        }
        if (node.get("node_id").equals(target)) {
          targetNode = node;
        }
      }
      for (String attribute : arcsFactsNames.subList(1, arcsFactsNames.size())) {
        String query = attribute + "(" + oneEdgeID + ",X)";
        Object value = Query.oneSolution(query).get("X"); // ibidem
        edgeMap.put(attribute, value);
      }

      graph.setArcValue(sourceNode, targetNode, edgeMap);
    }

    return graph;
  }

  public synchronized static PrologGraph applyOperator(String operator, PrologGraph parent, List<String> domainDefinition, List<String> domainStructuralRules) {
    // reset previous knowledge and change status
    resetPrologKnowledge();
    status = domainDefinition;


    // assert domainStructuralRules SWITCH WITH PARENT GRAPH?? (OK, but not enough)
    for (String rule : domainStructuralRules) {
      rule = rule.replace(".", "");
      rule = rule.replace(" ", "");
      Query.hasSolution("assert((" + rule + "))");
    }

    // Get parent-graph description and assert it on Prolog
    List<String> parentDescription = describeGraph(parent, domainDefinition);
    for (String fact : parentDescription) {
//            fact = fact.replace(".", ""); //
//            fact = fact.replace(" ", ""); // Since we built the graph description without spaces and punctuation, those two lines are removable
      Query.hasSolution("assert(" + fact + ").");
    }

    // Apply operator
    try {
      Query.hasSolution(operator);
    } catch (PrologException any) {
      return parent;
    }

    // check if is_valid is defined or define it
    List<String> rulesCheck = new ArrayList<>();
    for (String rule : domainStructuralRules) {
      rule = rule.replace(" ", "");
      int lengthSubstring = "is_valid:-".length();
      rulesCheck.add(rule.substring(0, lengthSubstring));
    }
    if (!rulesCheck.contains("is_valid:-"))
      Query.hasSolution("assert(( is_valid :- true )).");

    // Check validity of new graph
    String JPLValidity = "(jpl_valid(X) :- (is_valid -> X = true; X = false) )";
    Query.hasSolution("assert(" + JPLValidity + ")");

    //TODO: Error handling (WIP)
    if (Query.oneSolution("jpl_valid(X)").get("X").toString().equals("false")) {
      System.out.println("DEBUG: graph not valid, returning parent");
      return parent;
//      throw new UnsupportedOperationException("graph not valid");
    }

    return buildGraph(domainDefinition);
  }


  private static void resetPrologKnowledge() {
    if (status.isEmpty())
      return;
    int prefix = ":-dynamic".length();
    for (String fact : status) {
      fact = fact.replace(" ", ""); //remove spaces
      fact = fact.replace(".", ""); //remove point
      fact = fact.substring(prefix);                //extract fact name
      Query.hasSolution("abolish(" + fact + ").");
    }
  }

  private static List<String> extractFactsNames(List<String> domainDefinition) {
    List<String> facts = new ArrayList<>();
    int prefix = ":-dynamic".length();
    int suffix = "/n.".length();
    for (String term : domainDefinition) {
      term = term.replace(" ", ""); //remove  spaces
      term = term.substring(prefix, term.length() - suffix);
      facts.add(term);
    }
    return facts;
  }

  private static int getBreakpointNodesAndArcs(List<String> graphFactsNames) {
    int breakpoint = 0;
    for (int index = 0; index < graphFactsNames.size(); ++index) {
      if (graphFactsNames.get(index).equals("edge_id")) {
        breakpoint = index;
        break;
      }
    }
    return breakpoint;
  }


  public static PrologGraph generateGraph(String opAddNode, String opAddEdge, List<String> opPerturb, List<String> opRemove, List<String> domainDefinition, List<String> domainStructuralRules, int numRuns) {
    PrologGraph graph = new PrologGraph();

    if (numRuns < 5)
      throw new UnsupportedOperationException();

    // first add two nodes and an edge
    for (int i = 0; i < 2; ++i)
      graph = applyOperator(opAddNode, graph, domainDefinition, domainStructuralRules);
    graph = applyOperator(opAddNode, graph, domainDefinition, domainStructuralRules);

    // 5 random add
    List<String> opAdding = new ArrayList<>();
    opAdding.add(opAddEdge);
    opAdding.add(opAddNode);
    for (int i = 0; i < 5; ++i) {
      Random random = new Random();
      String randomOperator = opAdding.get(random.nextInt(opAdding.size()));
      graph = applyOperator(randomOperator, graph, domainDefinition, domainStructuralRules);
    }

    // 10 random with perturbations
    numRuns -= 8;
    if (numRuns <= 0)
      return graph;
    List<String> opNotRemove = new ArrayList<>();
    opNotRemove.addAll(opAdding);
    opNotRemove.addAll(opPerturb);
    for (int i = 0; i < Math.min(numRuns, 10); ++i) {
      Random random = new Random();
      String randomOperator = opNotRemove.get(random.nextInt(opNotRemove.size()));
      graph = applyOperator(randomOperator, graph, domainDefinition, domainStructuralRules);
    }

    // others random
    numRuns -= 10;
    if (numRuns < 0)
      return graph;
    List<String> operatorsAll = new ArrayList<>();
    operatorsAll.addAll(opNotRemove);
    operatorsAll.addAll(opRemove);
    for (int i = 0; i < numRuns; ++i) {
      Random random = new Random();
      String randomOperator = operatorsAll.get(random.nextInt(operatorsAll.size()));
      graph = applyOperator(randomOperator, graph, domainDefinition, domainStructuralRules);
    }

    return graph;
  }


  public static void main(String[] args) {


    // Operators' definition
    String operatorAddNodeWithAttribute = "gensym(nod,X), assert(node_id(X)), random(V), attribute_value(V), assert(attribute(X,V)).";

    String operatorAddEdgeWithAttribute = "findall(N,node_id(N), Nodes)," +
            "random_member(Source, Nodes)," +
            "random_member(Target, Nodes), " +
            "gensym(edg,E)," +
            "assert(edge_id(E))," +
            "assert(edge(Source,Target,E))," +
            "findall(C,colour_value(C),CDomain)," +
            "random_member(RED,CDomain)," +
            "assert(colour(E,RED)).";

    String operatorRemoveEdgeWithAttribute = "findall(EID,edge_id(EID),Ids)," +
            "random_member(Removable,Ids)," +
            "retract(edge_id(Removable))," +
            "retract(colour(Removable,_))," +
            "retract(edge(_,_,Removable)).";

    String operatorIntermediateNodeWithAttributes = "findall((S,T,I),edge(S,T,I),Edges)," +
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

    String operatorPerturbValue = "findall(X,node_id(X),Nodes)," +
            "random_member(N,Nodes)," +
            "attribute(N,V)," +
            "random(R)," +
            "New_Val is R*V," +
            "(attribute_value(New_Val) ->  retract(attribute(N,V))," +
            "                    assertz(attribute(N,New_Val));" +
            "    true).";

    String operatorModifyEdgeValue = "findall(ID,edge_id(ID),IDs)," +
            "random_member(E,IDs)," +
            "findall(C,colour_value(C),Colours)," +
            "random_member(V,Colours)," +
            "retract(colour(E,_))," +
            "assert(colour(E,V)).";

    // Subset of the graph:

    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.", ":- dynamic attribute/2.", ":- dynamic edge_id/1.", ":- dynamic edge/3.", ":- dynamic colour/2.");

    List<String> StructuralRules = Arrays.asList("is_valid :- true.",
            "retract_list([X | Xs], P) :- " +
                    "        Z =.. [P, X], retract(Z), retract_list(Xs, P).",
            "retract_list([], _) :- true.",
            "attribute_value(X) :- float(X), X =< 1, X >= 0.",
            "colour_value(red).",
            "colour_value(blue).",
            "colour_value(green)."
    );

    // Generation:
    List<String> perturbationOperators = new ArrayList<>();
    perturbationOperators.add(operatorPerturbValue);
    perturbationOperators.add(operatorModifyEdgeValue);
    perturbationOperators.add(operatorIntermediateNodeWithAttributes);

    List<String> removalOperators = new ArrayList<>();
    removalOperators.add(operatorRemoveEdgeWithAttribute);

    int numElements = 10;
    int numGenerations = 100;

    List<PrologGraph> graphGeneration = new ArrayList<>();
    double start = System.currentTimeMillis();
    for (int i = 0; i < numElements; ++i) {
      PrologGraph graph = generateGraph(operatorAddNodeWithAttribute, operatorAddEdgeWithAttribute, perturbationOperators, removalOperators, domainDefinition, StructuralRules, numGenerations);
      graphGeneration.add(graph);
    }
    double end = System.currentTimeMillis();
    System.out.println("Time needed to complete the whole for loop is " + (end - start) + "ms");
    System.out.println("Therefore, in the average one generation took " + ((end - start) / numElements) + "ms");




  }
}
