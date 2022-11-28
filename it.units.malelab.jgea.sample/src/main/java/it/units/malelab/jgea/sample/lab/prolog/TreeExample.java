package it.units.malelab.jgea.sample.lab.prolog;

import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.representation.graph.numeric.RealFunction;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologOperator;
import it.units.malelab.jgea.core.representation.graph.prolog.mapper.OperatorGraphMapper;
import it.units.malelab.jgea.core.selector.Last;
import it.units.malelab.jgea.core.selector.Tournament;
import it.units.malelab.jgea.core.solver.*;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.core.util.Misc;
import it.units.malelab.jgea.problem.symbolicregression.*;
import it.units.malelab.jgea.sample.lab.TuiExample;
import it.units.malelab.jgea.tui.TerminalMonitor;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Logger;

import static it.units.malelab.jgea.sample.lab.TuiExample.*;
import static it.units.malelab.jgea.core.listener.NamedFunctions.*;

public class TreeExample implements Runnable {
  private final static Logger L = Logger.getLogger(TuiExample.class.getName());

  private final ExecutorService executorService;


  private final int minDim;
  private final int maxDim;
  private final List<String> domainDefinition = Arrays.asList(
          ":- dynamic node_id/1.",
          ":- dynamic start/2.",
          ":- dynamic type/2.",
          ":- dynamic value/2.",
          ":- dynamic edge_id/1.",
          ":- dynamic edge/3.");
  private final List<String> structuralRules;
  private final PrologGraph originGraph;
  private final List<List<String>> opLabelsDescription;

  private final List<String> factoryOperators;

  public TreeExample(int minDim, int maxDim, List<String> factoryOperators, List<List<String>> opLabelsDescription, List<String> structuralRules) {
    executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() - 1);
    this.minDim = minDim;
    this.maxDim = maxDim;
    this.opLabelsDescription = opLabelsDescription;
    this.structuralRules = structuralRules;
    this.factoryOperators = factoryOperators;

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
    this.originGraph = origin;
  }

  @Override
  public void run() {
    TerminalMonitor<? super POSetPopulationState<?, ?, ? extends Double>, Map<String, Object>> tm =
            new TerminalMonitor<>(
                    Misc.concat(List.of(
                            BASIC_FUNCTIONS,
                            DOUBLE_FUNCTIONS,
                            List.of(solution().reformat("%25.100s").of(best()))
                    )),
                    List.of()
            );
    List<Integer> seeds = List.of(1, 2, 3, 4, 5);
    SyntheticSymbolicRegressionProblem p = new Polynomial2(SymbolicRegressionFitness.Metric.MSE);
    List<IterativeSolver<? extends POSetPopulationState<PrologGraph, RealFunction, Double>, SyntheticSymbolicRegressionProblem,
            RealFunction>> solvers = new ArrayList<>();

    Map<GeneticOperator<PrologGraph>, Double> operatorsMap = new HashMap<>();
    final double weight = 1.0d / opLabelsDescription.size();
    for (List<String> op : opLabelsDescription)
      operatorsMap.put(new PrologOperator(op.get(0), op.get(1), domainDefinition, structuralRules), weight);


    StandardEvolver stdEvolver = new StandardEvolver<>(
            new OperatorGraphMapper().andThen(og -> new RealFunction() {
              @Override
              public double apply(double... input) {
                return og.apply(input)[0];
              }

              public String toString() {
                return og.toString();
              }
            }),
            new PrologGraphFactory(minDim, maxDim, originGraph, factoryOperators, domainDefinition, structuralRules),
            100,
            StopConditions.nOfIterations(500),
            operatorsMap,
            new Tournament(5),
            new Last(),
            100,
            true,
            false,
            (srp, rnd) -> new POSetPopulationState<>()
    );

    solvers.add(stdEvolver);

    int counter = 0;
    for (int seed : seeds) {
      Random r = new Random(1);
      for (IterativeSolver<? extends POSetPopulationState<?, RealFunction, Double>, SyntheticSymbolicRegressionProblem,
              RealFunction> solver : solvers) {
        Map<String, Object> keys = Map.ofEntries(
                Map.entry("seed", seed),
                Map.entry("solver", solver.getClass().getSimpleName())
        );
        tm.notify((double) counter / (double) (seeds.size() * solvers.size()), "Starting " + keys);
        try {
          Collection<RealFunction> solutions = solver.solve(
                  p,
                  r,
                  executorService,
                  tm.build(keys).deferred(executorService)
          );
          counter = counter + 1;
          tm.notify((double) counter / (double) (seeds.size() * solvers.size()), "Starting " + keys);
          L.info(String.format("Found %d solutions with %s", solutions.size(), keys));
        } catch (SolverException e) {
          L.severe(String.format("Exception while doing %s: %s", e, keys));
          e.printStackTrace();
        }
      }
    }

    Map<PrologOperator, Integer> changes = stdEvolver.getChanges();
    Map<PrologOperator, Integer> usages = stdEvolver.getUsage();
    Set<PrologOperator> operatorsSet = changes.keySet();


    double sum = 0d;
    for (PrologOperator op : operatorsSet) {
      sum += usages.get(op);
    }

    String leftAlignFormat = "| %-15s | %-5d | %-1.3f | %-5d | %-1.3f |%n";
    System.out.format("+-----------------+-------+-------+-------+-------+%n");
    System.out.format("| Operator        |  use  |   %%   |  chg  |   %%   |%n");
    System.out.format("+-----------------+-------+-------+-------+-------+%n");
    for (PrologOperator op : operatorsSet) {
      System.out.printf(leftAlignFormat, op.getLabel(), usages.get(op), (double) usages.get(op) / sum, changes.get(op), ((double) changes.get(op)) / (usages.get(op)));
    }
    System.out.format("+-----------------+-------+-------+-------+-------+%n");

    System.out.println("Total usages: " + (int) sum);


    tm.shutdown();
  }


  public static void main(String[] args) {
    final List<String> structuralRules = Arrays.asList(
            "operator_val(+).",
            "operator_val(*).",
            "operator_val(-).",
            "operator_val(/).",
            "n_input(1).",
            "input_val(X) :- n_input(Max), integer(X), X>=0, X<Max.",
            "max_const(2.0).",
            "min_const(0.0).",
            "constant_val(X) :- max_const(Max), float(X), X>=0.0, X< Max.",
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

    //// Operators
    List<List<String>> operators = new ArrayList<>();
    List<String> factoryOperators = new ArrayList<>();

    String addSubTree = "findall(VV,(type(VV,input); type(VV,constant)),VAR)," +
            "random_member(V,VAR)," +
            "retract(value(V,_))," +
            "retract(type(V,_))," +
            "operator_val(OpVal)," +
            "assert(type(V,operator))," +
            "assert(value(V,OpVal))," +
            "gensym(nod,N1)," +
            "assert(node_id(N1))," +
            "assert(start(N1,0))," +
            "n_input(InpMax)," +
            "(   maybe ->  assert(type(N1,input))," +
            "                     random(0, InpMax, InpVal)," +
            "                     assert(value(N1,InpVal)); " +
            "    assert(type(N1,constant))," +
            "                     random(0,2.0,V1Val)," +
            "                     assert(value(N1,V1Val)) )," +
            "gensym(nod,N2)," +
            "assert(node_id(N2))," +
            "assert(start(N2,0))," +
            "(   maybe ->  assert(type(N2,input))," +
            "                     random(0, InpMax, InpVal2)," +
            "                     assert(value(N2,InpVal2)); " +
            "    assert(type(N2,constant))," +
            "                     random(0,2.0,V2Val)," +
            "                     assert(value(N2,V2Val)) )," +
            "gensym(edge,E1)," +
            "gensym(edge,E2)," +
            "assert(edge_id(E1))," +
            "assert(edge_id(E2))," +
            "assert(edge(N1,V,E1))," +
            "assert(edge(N2,V,E2)).";
    operators.add(Arrays.asList("addSubTree", addSubTree));
    factoryOperators.add(addSubTree);

    String changeOperator = "findall(OP,type(OP,operator), Operators)," +
            "random_member(O, Operators)," +
            "retract(value(O,_))," +
            "findall(V,operator_val(V),Values)," +
            "random_member(NewVal,Values)," +
            "assert(value(O,NewVal)).";
    operators.add(Arrays.asList("changeOperator", changeOperator));
    factoryOperators.add(changeOperator);

    String changeConstant = "findall(CON,type(CON,constant), Constants)," +
            "random_member(O, Constants)," +
            "retract(value(O,_))," +
            "random(0.000001,2.0,NewVal)," +
            "assert(value(O,NewVal)).";
    operators.add(Arrays.asList("changeConstant", changeConstant));

    String dropSubTree = "findall((Leaf1,Leaf2,Root,Edg1,Edg2) , ((type(Leaf1,constant);type(Leaf1,input)),edge(Leaf1,Root,Edg1),dif(Leaf1,Leaf2)," +
            "edge(Leaf2,Root,Edg2),(type(Leaf2,constant);type(Leaf2,input)))" +
            "        ,Leaves)," +
            "random_member((L1,L2,S,Edge1,Edge2),Leaves)," +
            "retract(edge(L1,S,Edge1))," +
            "retract(edge_id(Edge1))," +
            "retract(edge(L2,S,Edge2))," +
            "retract(edge_id(Edge2))," +
            "retract(node_id(L1))," +
            "retract(node_id(L2))," +
            "retract(start(L1,0))," +
            "retract(start(L2,0))," +
            "retract(value(L1,_))," +
            "retract(value(L2,_))," +
            "retract(type(L1,_))," +
            "retract(type(L2,_))," +
            "n_input(InpMax)," +
            "max_const(ConstMax)," +
            "min_const(ConstMin)," +
            "retract(type(S,_))," +
            "retract(value(S,_))," +
            "(   maybe ->  assert(type(S,input))," +
            "     random(0, InpMax, InpVal)," +
            "     assert(value(S,InpVal)); " +
            "assert(type(S,constant))," +
            "     random(ConstMin,ConstMax,V1Val)," +
            "     assert(value(S,V1Val)) ).";
    operators.add(Arrays.asList("dropSubTree", dropSubTree));

    String swapLeaves = "findall(VV,(type(VV,variable); type(VV,input) ),Leaves)," +
            "random_member(V1,Leaves)," +
            "random_member(V2,Leaves)," +
            "edge(V1,T1,Id1)," +
            "edge(V2,T2,Id2)," +
            "retract(edge(V1,T1,Id1))," +
            "retract(edge(V2,T2,Id2))," +
            "assert(edge(V1,T2,Id1))," +
            "assert(edge(V2,T1,Id2)).";
    operators.add(Arrays.asList("swapLeaves", swapLeaves));

    String constToInput = "findall(Con,type(Con,constant),Constants)," +
            "random_member(C,Constants)," +
            "retract(type(C,constant))," +
            "retract(value(C,_))," +
            "assert(type(C,input))," +
            "n_input(Max)," +
            "random(0,Max,NewVal)," +
            "assert(value(C,NewVal)).";
    operators.add(Arrays.asList("constToInput", constToInput));

    String inpToConst = "findall(Inp,type(Inp,input),Inputs)," +
            "random_member(I,Inputs)," +
            "retract(type(I,input))," +
            "retract(value(I,_))," +
            "assert(type(I,constant))," +
            "random(0.0000001,2.0,NewVal)," +
            "assert(value(I,NewVal)).";
    operators.add(Arrays.asList("inpToConst", inpToConst));

    new TreeExample(5, 37, factoryOperators, operators, structuralRules).run();
  }

}

