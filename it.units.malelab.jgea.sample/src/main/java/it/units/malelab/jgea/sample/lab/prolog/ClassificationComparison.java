package it.units.malelab.jgea.sample.lab.prolog;

import com.google.common.base.Stopwatch;
import it.units.malelab.jgea.core.listener.ListenerFactory;
import it.units.malelab.jgea.core.listener.NamedFunction;
import it.units.malelab.jgea.core.listener.TabularPrinter;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.representation.graph.numeric.RealFunction;
import it.units.malelab.jgea.core.representation.graph.numeric.functiongraph.BaseFunction;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologOperator;
import it.units.malelab.jgea.core.representation.graph.prolog.mapper.FunctionGraphMapper;
import it.units.malelab.jgea.core.selector.Last;
import it.units.malelab.jgea.core.selector.Tournament;
import it.units.malelab.jgea.core.solver.*;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.problem.classification.*;
import it.units.malelab.jgea.sample.Worker;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static it.units.malelab.jgea.core.listener.NamedFunctions.*;
import static it.units.malelab.jgea.sample.Args.*;

public class ClassificationComparison extends Worker {

  public ClassificationComparison(String[] args) {
    super(args);
  }

  public static void main(String[] args) {
    new ClassificationComparison(args);
  }

  @Override
  public void run() {
    List<String> ffnnBaseRules;
    try (Stream<String> ffnnRulesPath = Files.lines(Paths.get("./prolog/ffnn/structuralRules.txt"))) {
      ffnnBaseRules = ffnnRulesPath.collect(Collectors.toList());
      ffnnBaseRules.add(0, "max_weight(5.0).");
      ffnnBaseRules.add(0, "min_weight(-5.0).");
      ffnnBaseRules.add(0, "max_size(141).");
    } catch (IOException e) {
      throw new UnsupportedOperationException("Ffnn's structural rules not found in given path");
    }

    ClassificationFitness.Metric metric = ClassificationFitness.Metric.BALANCED_ERROR_RATE;

    // XOR
    int nInput = 2;
    int nOutput = 2;
    try {
      List<DatasetClassificationProblem> xorProblem = List.of(
              new DatasetClassificationProblem("./datasets/xor.csv", nOutput, "y", 10, 0, metric, metric)
      );
      List<String> ffnnRulesXor = new ArrayList<>(ffnnBaseRules);
      ffnnRulesXor.add(0, "n_input(" + nInput + ").");
      ffnnRulesXor.add(0, "n_output(" + nOutput + ").");
//      runSameDomain(ffnnRulesXor, xorProblem, nInput, nOutput, "XOR.csv");
    } catch (IOException any) {
      throw new UnsupportedOperationException("Error in XOR running");
    }

    // Iris
    nInput = 4;
    nOutput = 3;
    try {
      List<DatasetClassificationProblem> xorProblem = List.of(
              new DatasetClassificationProblem("./datasets/iris.csv", nOutput, "variety", 10, 0, metric, metric)
      );
      List<String> ffnnRulesXor = new ArrayList<>(ffnnBaseRules);
      ffnnRulesXor.add(0, "n_input(" + nInput + ").");
      ffnnRulesXor.add(0, "n_output(" + nOutput + ").");
      runSameDomain(ffnnRulesXor, xorProblem, nInput, nOutput, "IRIS.csv");
    } catch (IOException any) {
      throw new UnsupportedOperationException("Error in IRIS running");
    }


  }

  private void runSameDomain(List<String> ffnnStructuralRules, List<DatasetClassificationProblem> problems, int nInput, int nOutput, String filename) {
    final int nPop = i(a("nPop", "70"));
    final int nTournament = 5;
    final int diversityMaxAttempts = 100;
    final int nIterations = i(a("nIterations", "200"));
    final int[] seeds = ri(a("seed", "0:1"));

    final int minFactoryDim = 5 + 2 * (nOutput + nInput - 2);
    final int maxFactoryDim = 125;

    // Ffnn
    final PrologGraph ffnnOrigin = getFfnnOrigin(nInput, nOutput);
    final List<String> ffnnDomain = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic layer/2.",
            ":- dynamic bias/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic weight/2.");
    final String ffnnOperatorsPath = "./prolog/ffnn/operators/";
    List<List<String>> ffnnPrologOperatorsSelection = getLabelledOperators(ffnnOperatorsPath + "selection");
    List<List<String>> ffnnPrologOperatorsAll = getLabelledOperators(ffnnOperatorsPath + "others");
    ffnnPrologOperatorsAll.addAll(ffnnPrologOperatorsSelection);
    Map<GeneticOperator<PrologGraph>, Double> ffnnSelOperatorsMap = mapOperatorsEqualWeight(ffnnPrologOperatorsSelection, ffnnDomain, ffnnStructuralRules);
    Map<GeneticOperator<PrologGraph>, Double> ffnnAllOperatorsMap = mapOperatorsEqualWeight(ffnnPrologOperatorsAll, ffnnDomain, ffnnStructuralRules);
    // factories
    List<String> ffnnFactoryOperatorsAll = new ArrayList<>();
    List<String> ffnnFactoryOperatorsSelection = new ArrayList<>();
    try {
      final List<String> ffnnFactorySelection = Arrays.asList("addEdge.txt", "addFinalLayer.txt", "addInitialLayer.txt", "addNode.txt"); //selection
      for (String fileName : ffnnFactorySelection) {
        ffnnFactoryOperatorsSelection.add(Files.readString(Path.of(ffnnOperatorsPath + "selection/" + fileName)));
      }
      final List<String> ffnnFactoryOthers = Arrays.asList("addConnectedNode.txt", "addNodeAndEdge.txt"); //others
      for (String fileName : ffnnFactoryOthers) {
        ffnnFactoryOperatorsAll.add(Files.readString(Path.of(ffnnOperatorsPath + "others/" + fileName)));
      }
      ffnnFactoryOperatorsAll.addAll(ffnnFactoryOperatorsSelection);
    } catch (IOException any) {
      throw new UnsupportedOperationException("IOException in ffnn factories creation.");
    }

    NamedFunction<List<Double>, Double> f0Extractor = doubles -> doubles.get(0);

    // Consumers
    List<NamedFunction<? super POSetPopulationState<?, ?, ? extends List<Double>>, ?>> functions = List.of(
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
            fitnessMappingIteration().of(best()),
            best().then(fitness()).then(vs -> ((List<Double>) vs).get(0)),
            hist(8).of(each(size().of(genotype()))).of(all()),
            max(Comparator.comparingDouble(Number::doubleValue)).reformat("%3d").of(each(size().of(genotype()))).of(all()),
            min(Comparator.comparingDouble(Number::doubleValue)).reformat("%3d").of(each(size().of(genotype()))).of(all()),
            operatorsProbabilitiesPlot(20),
            operatorsProbabilitiesAll(),
            solution().reformat("%80.80s").of(best())
    );

    List<NamedFunction<? super Map<String, Object>, ?>> kFunctions = List.of(
            attribute("seed").reformat("%2d"),
            attribute("problem").reformat(NamedFunction.formatOfLongest(problems.stream()
                    .map(p -> p.getClass().getSimpleName())
                    .toList())),
            attribute("evolver").reformat("%20.20s")
    );
    ListenerFactory<POSetPopulationState<?, ?, ? extends List<Double>>, Map<String, Object>> listenerFactory =
            new TabularPrinter<>(
                    functions,
                    kFunctions
            );
//    listenerFactory = ListenerFactory.all(List.of(
//            listenerFactory,
//            new CSVPrinter<>(functions, kFunctions, new File("./prolog/results/" + filename))
//    ));


    // Evolvers
    Map<String, Function<DatasetClassificationProblem, IterativeSolver<? extends POSetPopulationState<?, MRFClassifier, List<Double>>,
            DatasetClassificationProblem, Classifier<double[], Integer>>>> solvers = new TreeMap<>();

    solvers.put("prolog-enfdiv-all", p -> new StandardWithEnforcedDiversityEvolver(
            (new FunctionGraphMapper(BaseFunction.TANH).andThen(MRFClassifier::new)),
            new PrologGraphFactory(minFactoryDim, maxFactoryDim, ffnnOrigin, ffnnFactoryOperatorsAll, ffnnDomain, ffnnStructuralRules),
            nPop,
            StopConditions.nOfIterations(nIterations),
            ffnnAllOperatorsMap,
            new Tournament(nTournament),
            new Last(),
            nPop,
            true,
            false,
            (srp, r) -> new POSetPopulationState<>(),
            diversityMaxAttempts
    ));

//    solvers.put("prolog-enfdiv-sel", p -> new StandardWithEnforcedDiversityEvolver(
//            (new FunctionGraphMapper(BaseFunction.TANH).andThen(MRFClassifier::new)),
//            new PrologGraphFactory(minFactoryDim, maxFactoryDim, ffnnOrigin, ffnnFactoryOperatorsSelection, ffnnDomain, ffnnStructuralRules),
//            nPop,
//            StopConditions.nOfIterations(nIterations),
//            ffnnSelOperatorsMap,
//            new Tournament(nTournament),
//            new Last(),
//            nPop,
//            true,
//            false,
//            (srp, r) -> new POSetPopulationState<>(),
//            diversityMaxAttempts
//    ));
//
//    Function<Long, Double> constSchedule = x -> 0.01d;
//    solvers.put("prolog-adaptive", p -> new AdaptiveEvolver(
//            (new FunctionGraphMapper(BaseFunction.TANH).andThen(MRFClassifier::new)),
//            new PrologGraphFactory(minFactoryDim, maxFactoryDim, ffnnOrigin, ffnnFactoryOperatorsAll, ffnnDomain, ffnnStructuralRules),
//            nPop,
//            StopConditions.nOfIterations(nIterations),
//            ffnnAllOperatorsMap,
//            new Tournament(nTournament),
//            new Last(),
//            nPop,
//            true,
//            false,
//            diversityMaxAttempts,
//            constSchedule
//    ));


    L.info(String.format("Going to test with %d evolvers: %s%n", solvers.size(), solvers.keySet()));
    //run
    for (int seed : seeds) {
      for (DatasetClassificationProblem problem : problems) {
        for (Map.Entry<String, Function<DatasetClassificationProblem, IterativeSolver<? extends POSetPopulationState<?, MRFClassifier, List<Double>>,
                DatasetClassificationProblem, Classifier<double[], Integer>>>> solverEntry :
                solvers.entrySet()) {
          Map<String, Object> keys = Map.ofEntries(
                  Map.entry("seed", seed),
                  Map.entry("problem", problem.getClass().getSimpleName().toLowerCase()),
                  Map.entry("evolver", solverEntry.getKey())
          );
          try {
            Stopwatch stopwatch = Stopwatch.createStarted();
            IterativeSolver solver = solverEntry.getValue()
                    .apply(problem);
            L.info(String.format("Starting %s", keys));
            Collection<RealFunction> solutions = solver.solve(
                    problem,
                    new Random(seed),
                    executorService,
                    listenerFactory.build(keys).deferred(executorService)
            );
            L.info(String.format(
                    "Done %s: %d solutions in %4.1fs",
                    keys,
                    solutions.size(),
                    (double) stopwatch.elapsed(TimeUnit.MILLISECONDS) / 1000d
            ));
          } catch (SolverException e) {
            L.severe(String.format("Cannot complete %s due to %s", keys, e));
            e.printStackTrace();
          }
        }
      }
    }
    listenerFactory.shutdown();
  }

  private List<List<String>> getLabelledOperators(String pathToFolder) {
    List<List<String>> labelledOperators = new ArrayList<>();
    try {
      final File folder = new File(pathToFolder);
      File[] files = folder.listFiles();
      if (files == null) {
        throw new UnsupportedOperationException("No files defined in " + pathToFolder);
      } else {
        for (File file : files) {
          String operator = Files.readString(file.toPath());
          labelledOperators.add(Arrays.asList(file.getName().replace(".txt", ""), operator));
        }
      }
    } catch (IOException ioException) {
      throw new UnsupportedOperationException("IOException in trees' factories.");
    }
    return labelledOperators;
  }

  private Map<GeneticOperator<PrologGraph>, Double> mapOperatorsEqualWeight
          (List<List<String>> prologOperators, List<String> domainDefinition, List<String> structuralRules) {
    Map<GeneticOperator<PrologGraph>, Double> operatorsMap = new HashMap<>();
    final double weightSel = 1.0d / prologOperators.size();
    for (List<String> op : prologOperators)
      operatorsMap.put(new PrologOperator(op.get(0), op.get(1), domainDefinition, structuralRules), weightSel);
    return operatorsMap;
  }

  private PrologGraph getFfnnOrigin(int nInput, int nOutput) {
    PrologGraph ffnn = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("layer", 0);
    node1.put("bias", 0d);
    LinkedHashMap<String, Object> node2 = new LinkedHashMap<>();
    node2.put("node_id", "second");
    node2.put("layer", 1);
    node2.put("bias", 1.0d);
    LinkedHashMap<String, Object> node3 = new LinkedHashMap<>();
    node3.put("node_id", "third");
    node3.put("layer", 2);
    node3.put("bias", 0d);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "firstEdge");
    edge1.put("weight", 0.5d);
    LinkedHashMap<String, Object> edge2 = new LinkedHashMap<>();
    edge2.put("edge_id", "secondEdge");
    edge2.put("weight", 0.2d);
    ffnn.addNode(node1);
    ffnn.addNode(node2);
    ffnn.addNode(node3);
    ffnn.setArcValue(node1, node2, edge1);
    ffnn.setArcValue(node2, node3, edge2);
    for (int i = 1; i < nInput; ++i) {
      LinkedHashMap<String, Object> node = new LinkedHashMap<>();
      node.put("node_id", "first" + i);
      node.put("layer", 0);
      node.put("bias", 0);
      ffnn.addNode(node);
      LinkedHashMap<String, Object> edge = new LinkedHashMap<>();
      edge.put("edge_id", "edge" + i);
      edge.put("weight", 0.5d);
      ffnn.setArcValue(node, node2, edge);
    }
    for (int i = 1; i < nOutput; ++i) {
      LinkedHashMap<String, Object> node = new LinkedHashMap<>();
      node.put("node_id", "last" + i);
      node.put("layer", 2);
      node.put("bias", 0);
      ffnn.addNode(node);
      LinkedHashMap<String, Object> edge = new LinkedHashMap<>();
      edge.put("edge_id", "edgeLast" + i);
      edge.put("weight", 0.5d);
      ffnn.setArcValue(node2, node, edge);
    }
    return ffnn;
  }

}
