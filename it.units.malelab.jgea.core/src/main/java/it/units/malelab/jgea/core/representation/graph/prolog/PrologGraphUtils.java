package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.*;


import java.util.*;

public class PrologGraphUtils {

  static List<String> status = new ArrayList<>();

  static List<String> domainStatus = new ArrayList<>();

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
    Variable N = new Variable("N");
    Query nodeIDQuery = new Query("node_id", new Term[]{N});
    if (!nodeIDQuery.hasSolution()) //if no node id => empty graph
      return graph;
    Map<String, Term>[] allNodeIDs = nodeIDQuery.allSolutions();

    // Create list of nodes
    List<LinkedHashMap<String, Object>> nodesCollection = new ArrayList<>();

    // Add nodes to graph
    LinkedHashMap<String, Object> nodeMap;
    for (Map<String, Term> nodeIDMap : allNodeIDs) { // iterating through nodeIDs
      String oneNodeID = nodeIDMap.get("N").toString();
      nodeMap = new LinkedHashMap<>();
      nodeMap.put("node_id", oneNodeID);

      Variable V = new Variable("V");
      for (String attribute : nodesFactsNames.subList(1, nodesFactsNames.size())) { // iterating through attributes
        Object value = Query.oneSolution(attribute, new Term[]{new Atom(oneNodeID), V}).get("V");
//        String query = attribute + "(" + oneNodeID + ",X)";
//        Object value = Query.oneSolution(query).get("X");
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

    // get parent-graph description and assert it on Prolog
    List<String> parentDescription = describeGraph(parent, domainDefinition);
    for (String fact : parentDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    // assert domainStructuralRules -> PROBLEM WHEN GRAPH IS EMPTY
//    if (!domainStatus.equals(domainStructuralRules)) { // with IF it goes slower... wtf
      retractRules(domainStatus); // retract previous ones
      domainStatus = domainStructuralRules;
      for (String rule : domainStructuralRules) {
        rule = rule.replace(".", "");
        Query.hasSolution("assert((" + rule + "))");
      }
//    }

    // apply operator
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


    System.out.println("DEBUG: PrologGraphUtils before check on validity");
    // check validity (updated)
    if (!Query.hasSolution("is_valid")) {
      return parent;
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

  private static void retractRules(List<String> domainStructuralRules) {
    List<String> rulesSet = new ArrayList<>();
    for (String rule : domainStructuralRules) {
      if (!rule.contains(".")) {
        throw new UnsupportedOperationException("ERROR, rule defined without ending point.");
      }
      int arity;
      String query = "/";
      rule = rule.replace(" ", "");

      if (rule.contains(":-")) {
        int index = rule.indexOf(":-");
        rule = rule.substring(0, index);
      }

      if (!rule.contains(")")) {
        arity = 0;
      } else {
        int count = rule.length() - rule.replace(",", "").length(); // compute occurrences of ","
        arity = 1 + count;
        rule = rule.substring(0, rule.indexOf("("));
      }

      query = rule + query + arity;
      if (!rulesSet.contains(query))
        rulesSet.add(query);
    }

    for (String ruleAndArity : rulesSet) {
      Query.hasSolution("abolish(" + ruleAndArity + ").");
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


}
