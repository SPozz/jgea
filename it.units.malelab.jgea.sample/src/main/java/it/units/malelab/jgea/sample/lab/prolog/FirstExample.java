package it.units.malelab.jgea.sample.lab.prolog;

import it.units.malelab.jgea.core.representation.graph.numeric.RealFunction;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;
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

public class FirstExample implements Runnable {

  // Basic_functions and double_functions from TuiExample
  private final static Logger L = Logger.getLogger(TuiExample.class.getName());
  private final ExecutorService executorService;
  private final int minDim;
  private final int maxDim;
  private final List<String> domainDefinition = new ArrayList<>();
  private final List<String> structuralRules;
  private final PrologGraph originGraph = new PrologGraph();
  private final List<String> operators = new ArrayList<>();

  public FirstExample(int minDim, int maxDim, List<String> structuralRules) {
    executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() - 1);
    this.minDim = minDim;
    this.maxDim = maxDim;
    this.structuralRules = structuralRules;
  }


  public static void main(String[] args) {
    new FirstExample(10, 20, Arrays.asList("", "")).run();
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
    solvers.add(new StandardEvolver<>(
            new OperatorGraphMapper().andThen(og -> (RealFunction) input -> og.apply(input)[0]),
            new PrologGraphFactory(minDim, maxDim, originGraph, operators, domainDefinition, structuralRules),
            100,
            StopConditions.nOfIterations(500),
            null,//Map.of(new SameRootSubtreeCrossover<>(12), 0.8d, new GrammarBasedSubtreeMutation<>(12, srGrammar), 0.2d),
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

