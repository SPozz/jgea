package it.units.malelab.jgea.sample.lab.prolog;

import it.units.malelab.jgea.core.listener.NamedFunction;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.representation.graph.numeric.RealFunction;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologOperator;
import it.units.malelab.jgea.core.representation.graph.prolog.mapper.DeterministicFiniteAutomatonMapper;
import it.units.malelab.jgea.core.selector.Last;
import it.units.malelab.jgea.core.selector.Tournament;
import it.units.malelab.jgea.core.solver.IterativeSolver;
import it.units.malelab.jgea.core.solver.SolverException;
import it.units.malelab.jgea.core.solver.StandardEvolver;
import it.units.malelab.jgea.core.solver.StopConditions;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.core.util.Misc;
import it.units.malelab.jgea.problem.symbolicregression.Polynomial2;
import it.units.malelab.jgea.problem.symbolicregression.SymbolicRegressionFitness;
import it.units.malelab.jgea.problem.symbolicregression.SyntheticSymbolicRegressionProblem;
import it.units.malelab.jgea.sample.lab.TuiExample;
import it.units.malelab.jgea.tui.TerminalMonitor;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static it.units.malelab.jgea.core.listener.NamedFunctions.*;
import static it.units.malelab.jgea.core.listener.NamedFunctions.all;

public class FsmSingleProblem implements Runnable {
  private final static Logger L = Logger.getLogger(TuiExample.class.getName());
  private final ExecutorService executorService;

  private final int minDim;
  private final int maxDim;
  private final List<String> domainDefinition = Arrays.asList(
          ":- dynamic node_id/1.",
          ":- dynamic start/2.",
          ":- dynamic accepting/2.",
          ":- dynamic edge_id/1.",
          ":- dynamic edge/3.",
          ":- dynamic input/2.");
  private final List<String> structuralRules;
  private final PrologGraph originGraph;
  private final List<List<String>> opLabelsDescription;

  private final List<String> factoryOperators;

  public final static List<NamedFunction<? super POSetPopulationState<?, ?, ?>, ?>> BASIC_FUNCTIONS =
          List.of(
                  iterations(),
                  births(),
                  elapsedSeconds(),
                  size().of(all()),
                  size().of(firsts()),
                  size().of(lasts()),
                  uniqueness().of(each(genotype())).of(all()),
                  uniqueness().of(each(solution())).of(all()),
                  uniqueness().of(each(fitness())).of(all()),
                  size().of(genotype()).of(best()),
                  fitnessMappingIteration().of(best())
          );

  public final static List<NamedFunction<? super POSetPopulationState<?, ?, ? extends Double>, ?>> DOUBLE_FUNCTIONS =
          List.of(
                  fitness().reformat("%5.3f").of(best()),
                  hist(8).of(each(fitness())).of(all()),
                  max(Double::compare).reformat("%5.3f").of(each(fitness())).of(all())
          );

  public FsmSingleProblem(int minDim, int maxDim, List<String> factoryOperators, List<List<String>> opLabelsDescription, List<String> structuralRules) {
    executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() - 1);

    this.minDim = minDim;
    this.maxDim = maxDim;
    this.opLabelsDescription = opLabelsDescription;
    this.structuralRules = structuralRules;
    this.factoryOperators = factoryOperators;

    PrologGraph origin = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("start",1);
    node1.put("accepting",1);

    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "loopEdge");
    edge1.put("input","[0,1]");

    origin.addNode(node1);
    origin.setArcValue(node1, node1, edge1);
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
    List<Integer> seeds = List.of(1);//, 2, 3, 4, 5);
    SyntheticSymbolicRegressionProblem p = new Polynomial2(SymbolicRegressionFitness.Metric.MSE);
    List<IterativeSolver<? extends POSetPopulationState<PrologGraph, RealFunction, Double>, SyntheticSymbolicRegressionProblem,
            RealFunction>> solvers = new ArrayList<>();

    Map<GeneticOperator<PrologGraph>, Double> operatorsMapFsm = new HashMap<>();
    final double weight = 1.0d / opLabelsDescription.size();
    for (List<String> op : opLabelsDescription)
      operatorsMapFsm.put(new PrologOperator(op.get(0), op.get(1), domainDefinition, structuralRules), weight);

    StandardEvolver stdEvolver = new StandardEvolver<>(
            new DeterministicFiniteAutomatonMapper().andThen(fg -> new RealFunction() {
              @Override
              public double apply(double... input) {
                return fg.size();//TODO: not size, something else
              }

              public String toString() {
                return fg.toString();
              }
            }),
            new PrologGraphFactory(minDim, maxDim, originGraph, factoryOperators, domainDefinition, structuralRules),
            100,
            StopConditions.nOfIterations(50),
            operatorsMapFsm,
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

    String leftAlignFormat = "| %-20s | %-5d | %-1.3f | %-5d | %-1.3f |%n";
    System.out.format("+----------------------+-------+-------+-------+-------+%n");
    System.out.format("| Operator             |  use  |   %%   |  chg  |   %%   |%n");
    System.out.format("+----------------------+-------+-------+-------+-------+%n");
    for (PrologOperator op : operatorsSet) {
      System.out.printf(leftAlignFormat, op.getLabel(), usages.get(op), (double) usages.get(op) / sum, changes.get(op), ((double) changes.get(op)) / (usages.get(op)));
    }
    System.out.format("+----------------------+-------+-------+-------+-------+%n");

    System.out.println("Total usages: " + (int) sum);

    try {
      TimeUnit.SECONDS.sleep(10);
    } catch (InterruptedException any) {
      System.out.println("InterruptedException");
    }

    tm.shutdown();
  }


  public static void main(String[] args) {
    String folder = "./prolog/fsm/operators/";
    File selectionOperatorsFolder = new File(folder + "selection");
    File othersOperatorsFolder = new File(folder + "others");
    final File factoryFolder = new File(folder + "factory");

    List<List<String>> operators = new ArrayList<>();
    List<String> factoryOperators = new ArrayList<>();
    List<String> structuralRules;

    // structuralRules
    try (Stream<String> rulesPath = Files.lines(Paths.get("./prolog/fsm/structuralRules.txt"))) {
      structuralRules = rulesPath.collect(Collectors.toList());
    } catch (IOException e) {
      throw new UnsupportedOperationException("structural rules not found in given path");
    }

    try {
      // operators
      File[] filesSel = selectionOperatorsFolder.listFiles();
      if (filesSel != null) {
        for (File file : filesSel) {
          String operator = Files.readString(file.toPath());
          operators.add(Arrays.asList(file.getName().replace(".txt", ""), operator));
        }
      } else {
        System.out.println("No files defined in operator selection. Using only others.");
      }
      File[] filesOthers = othersOperatorsFolder.listFiles();
      if (filesOthers != null) {
        for (File file : filesOthers) {
          String operator = Files.readString(file.toPath());
          operators.add(Arrays.asList(file.getName().replace(".txt", ""), operator));
        }
      }
      // factory
      File[] factoryFiles = factoryFolder.listFiles();
      if (factoryFiles != null) {
        for (File file : factoryFiles) {
          factoryOperators.add(Files.readString(file.toPath()));
        }
      } else {
        throw new UnsupportedOperationException("No files defined in factory");
      }
    } catch (IOException any) {
      throw new UnsupportedOperationException("IOException in main.");
    }


    final int nSymbols = 2;
    for (int i = 0; i < nSymbols; ++i) {
      structuralRules.add(0, "input_val(" + i + ").");
    }

    new FsmSingleProblem(5, 20, factoryOperators, operators, structuralRules).run();
  }


}