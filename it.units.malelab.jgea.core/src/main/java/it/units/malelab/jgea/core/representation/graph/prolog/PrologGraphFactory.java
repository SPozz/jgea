package it.units.malelab.jgea.core.representation.graph.prolog;

import it.units.malelab.jgea.core.Factory;
import it.units.malelab.jgea.core.util.Misc;

import java.util.*;
import java.util.random.RandomGenerator;

public class PrologGraphFactory implements Factory<PrologGraph> {

  final private int minDimension;
  final private int maxDimension;
  final private PrologGraph originGraph;
  final private List<String> operators;
  final private List<String> domainDefinition;
  final private List<String> structuralRules;


  PrologGraphFactory(int minDimension, int maxDimension, PrologGraph originGraph, List<String> operators, List<String> domainDefinition, List<String> structuralRules) {
    this.minDimension = minDimension;
    this.maxDimension = maxDimension;
    this.originGraph = originGraph;
    this.domainDefinition = domainDefinition;
    this.structuralRules = structuralRules;
    this.operators = operators;
  }

  @Override
  public List<PrologGraph> build(int n, RandomGenerator random) {
    List<PrologGraph> graphList = new ArrayList<>();
    final int maxTries = 100;

    for (int i = 0; i < n; ++i) {
      final int dimension = random.nextInt(minDimension, maxDimension + 1);
      PrologGraph graph = originGraph;
      int nTries = 0;
      while (graph.size() < dimension && nTries < maxTries) { // Less or Different?

        String operator = Misc.pickRandomly(operators, random);
        graph = PrologGraphUtils.applyOperator(operator, graph, domainDefinition, structuralRules);

        nTries++;
      }
      graphList.add(graph);
    }

    return graphList;
  }

  public static List<PrologGraph> test(int n, RandomGenerator random, int minDimension, int maxDimension, PrologGraph originGraph, List<String> operators, List<String> domainDefinition, List<String> structuralRules) {
    List<PrologGraph> graphList = new ArrayList<>();
    final int maxTries = 100;

    for (int i = 0; i < n; ++i) {
      final int dimension = random.nextInt(minDimension, maxDimension + 1);
//      System.out.println("dimension prevista: "+dimension);
      PrologGraph graph = originGraph;
      int nTries = 0;
      while (graph.size() < dimension && nTries < maxTries) { // Check if "<" or "!=" (but then add removal in case size >)

        String operator = Misc.pickRandomly(operators, random);
        graph = PrologGraphUtils.applyOperator(operator, graph, domainDefinition, structuralRules);

        nTries++;
      }
      graphList.add(graph);
    }

    return graphList;
  }


  public static void main(String[] args) {
    final Random random = new Random();

    PrologGraph origin = new PrologGraph();
    PrologGraph singleton = new PrologGraph();
    Map<String,Object> node = new HashMap<>();
    node.put("node_id","singleton");
    node.put("attribute",0.123);
    singleton.addNode(node);


    // Domain
    final List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.", ":- dynamic attribute/2.", ":- dynamic edge_id/1.", ":- dynamic edge/3.", ":- dynamic colour/2.");
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
    final List<String> operators = Arrays.asList(operatorAddEdgeWithAttribute, operatorAddNodeWithAttribute, operatorModifyEdgeValue, operatorPerturbValue, operatorIntermediateNodeWithAttributes);





    // Actual TEST
    int nGraphs = 5;
    List<PrologGraph> testing = test(nGraphs, random, 10, 12, singleton, operators, domainDefinition, structuralRules);//origin or singleton
    for (PrologGraph graph : testing) {
      System.out.println(graph.size());
    }

    System.out.println("\nFirst one generated. Nodes and arcs:");
    PrologGraph exmp0 = testing.get(0);
    System.out.println(exmp0.nodes());
    System.out.println(exmp0.arcs());

    System.out.println("\nLast one generated. Nodes and arcs:");
    PrologGraph exmpLast = testing.get(nGraphs-1);
    System.out.println(exmpLast.nodes());
    System.out.println(exmpLast.arcs());

  }


}
