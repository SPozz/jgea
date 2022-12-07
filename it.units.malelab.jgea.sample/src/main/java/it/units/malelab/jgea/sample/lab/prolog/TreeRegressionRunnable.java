package it.units.malelab.jgea.sample.lab.prolog;

import it.units.malelab.jgea.core.listener.NamedFunction;
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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static it.units.malelab.jgea.core.listener.NamedFunctions.*;

public class TreeRegressionRunnable implements Runnable {

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

  public TreeRegressionRunnable(int minDim, int maxDim, List<String> factoryOperators, List<List<String>> opLabelsDescription, List<String> structuralRules) {
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
    node2.put("value", 0.5d); // con + 1 è facile..
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
    SyntheticSymbolicRegressionProblem p = new Polynomial3(SymbolicRegressionFitness.Metric.MSE);
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
    String folder = "./prolog/trees/operators/";
    File selectionOperatorsFolder = new File(folder + "selection");
    File allOperatorsFolder = new File(folder + "others");
    final File folderFactory = new File(folder + "factory");


    List<String> factoryFiles = Arrays.asList("addSubTree.txt", "innerSubTree.txt");
    List<List<String>> operators = new ArrayList<>();
    List<String> factoryOperators = new ArrayList<>();
    List<String> structuralRules;

    // structuralRules
    try (Stream<String> rulesPath = Files.lines(Paths.get("./prolog/trees/structuralRules.txt"))) {
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
        throw new UnsupportedOperationException("No files defined in operator selection");
      }
      File[] filesOthers = allOperatorsFolder.listFiles();
      if (filesOthers != null) {
        for (File file : filesOthers) {
          String operator = Files.readString(file.toPath());
          operators.add(Arrays.asList(file.getName().replace(".txt", ""), operator));
        }
      }
      // factory
      for (String fileName : factoryFiles) {
        factoryOperators.add(Files.readString(Path.of(folderFactory + "/" + fileName)));
      }
    } catch (IOException any) {
      throw new UnsupportedOperationException("IOException in main.");
    }

    final double minConst = 0;
    final double maxConst = 2.0d;
    final int nInput = 1;
    structuralRules.add(0, "n_input(" + nInput + ").");
    structuralRules.add(0, "max_const(" + maxConst + ").");
    structuralRules.add(0, "min_const(" + minConst + ").");

    new TreeRegressionRunnable(5, 29, factoryOperators, operators, structuralRules).run();


  }

}

