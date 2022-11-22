package it.units.malelab.jgea.sample.lab.prolog;

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
import it.units.malelab.jgea.problem.symbolicregression.Nguyen7;
import it.units.malelab.jgea.problem.symbolicregression.SymbolicRegressionFitness;
import it.units.malelab.jgea.problem.symbolicregression.SyntheticSymbolicRegressionProblem;
import it.units.malelab.jgea.sample.lab.TuiExample;
import it.units.malelab.jgea.tui.TerminalMonitor;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Logger;

import static it.units.malelab.jgea.sample.lab.TuiExample.*;

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
  private final List<String> operators;

  public TreeExample(int minDim, int maxDim, List<String> operators, List<String> structuralRules) {
    executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() - 1);
    this.minDim = minDim;
    this.maxDim = maxDim;
    this.operators = operators;
    this.structuralRules = structuralRules;

    PrologGraph origin = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("start", 1);
    node1.put("type", "operator");
    node1.put("value", "+");
    LinkedHashMap<String, Object> node2 = new LinkedHashMap<>();
    node2.put("node_id", "second");
    node2.put("start", 0);
    node2.put("type", "variable");
    node2.put("value", 5);
    LinkedHashMap<String, Object> node3 = new LinkedHashMap<>();
    node3.put("node_id", "third");
    node3.put("start", 0);
    node3.put("type", "variable");
    node3.put("value", 3);
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


  public static void main(String[] args) {
    List<String> structuralRules = Arrays.asList(
            "variable_val(X) :- integer(X), X>= 0, X < 10.",
            "operator_val(+).",
            "operator_val(*).",
            "operator_val(-).",
            "operator_val(/).",
            "start_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 0.",
            "node_outdegree(S) :- findall(E, edge(S,_,E), RES), length(RES,N1), N1 == 1.",
            "operator_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 2.",
            "variable_indegree(T) :- findall(E, edge(_,T,E), RES), length(RES,N1), N1 == 0.",
            "check_start :- findall(N,start(N,1), N),length(N,N1), N1 == 1.",
            "start_connected(N) :- start(N,1).",
            "start_connected(N) :- edge(N,X,_), start_connected(X).",
            "is_valid :- check_start," +
                    "    foreach(findall(N,node_id(N),Node),maplist(start_connected,Node))," +
                    "    foreach(findall(T,(node_id(T),start(T,1)),T), maplist(start_outdegree,T))," +
                    "    foreach(findall(T,(node_id(T),start(T,0)),T), maplist(node_outdegree,T))," +
                    "    foreach(findall(O,type(O,operator),O), maplist(operator_indegree,O))," +
                    "    foreach(findall(V,type(V,variable),V), maplist(variable_indegree,V)).");

    //// Operators
    List<String> operators = new ArrayList<>();

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
            "random_between(0,9,V1Val)," +
            "assert(value(N1,V1Val))," +
            "assert(start(N1,0))," +
            "gensym(nod,N2)," +
            "assert(node_id(N2))," +
            "assert(type(N2,variable))," +
            "random_between(0,9,V2Val)," +
            "assert(value(N2,V2Val))," +
            "assert(start(N2,0))," +
            "gensym(edge,E1)," +
            "gensym(edge,E2)," +
            "assert(edge_id(E1))," +
            "assert(edge_id(E2))," +
            "assert(edge(N1,V,E1))," +
            "assert(edge(N2,V,E2)).";
    operators.add(subTree);

    String perturbOperator = "findall(OP,type(OP,operator), Operators)," +
            "random_member(O, Operators)," +
            "retract(value(O,_))," +
            "findall(V,operator_val(V),Values)," +
            "random_member(X,Values)," +
            "assert(value(O,X))";
    operators.add(perturbOperator);

    String perturbVariable = "findall(VAR,type(VAR,variable), Variables)," +
            "random_member(O, Variables)," +
            "retract(value(O,_))," +
            "random_between(0,9,X)," +
            "assert(value(O,X))";
    operators.add(perturbVariable);

    new TreeExample(10, 30, operators, structuralRules).run();
  }


  public void run() {
    TerminalMonitor<? super POSetPopulationState<?, ?, ? extends Double>, Map<String, Object>> tm =
            new TerminalMonitor<>(
                    Misc.concat(List.of(BASIC_FUNCTIONS, DOUBLE_FUNCTIONS)),
                    List.of()
            );
    List<Integer> seeds = List.of(1, 2, 3, 4, 5);
    SyntheticSymbolicRegressionProblem p = new Nguyen7(SymbolicRegressionFitness.Metric.MSE, 1);
    List<IterativeSolver<? extends POSetPopulationState<PrologGraph, RealFunction, Double>, SyntheticSymbolicRegressionProblem,
            RealFunction>> solvers = new ArrayList<>();

    Map<PrologOperator, Double> operatorsMap = new HashMap<>();
    final double weight = 1.0d / operators.size();
    for (String op : operators)
      operatorsMap.put(new PrologOperator(op, domainDefinition, structuralRules), weight);

    solvers.add(new StandardEvolver<>(
            new OperatorGraphMapper().andThen(og -> (RealFunction) input -> og.apply(input)[0]),
            new PrologGraphFactory(minDim, maxDim, originGraph, operators, domainDefinition, structuralRules),
            100,
            StopConditions.nOfIterations(500),
            null, //operatorsMap
            new Tournament(5),
            new Last(),
            100,
            true,
            false,
            (srp, rnd) -> new POSetPopulationState<>()
    ));

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
        }
      }
    }
    tm.shutdown();
  }
}

