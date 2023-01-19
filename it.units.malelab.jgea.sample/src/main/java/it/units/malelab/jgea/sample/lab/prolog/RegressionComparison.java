package it.units.malelab.jgea.sample.lab.prolog;

import com.google.common.base.Stopwatch;
import it.units.malelab.jgea.core.IndependentFactory;
import it.units.malelab.jgea.core.listener.CSVPrinter;
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
import it.units.malelab.jgea.core.representation.graph.prolog.mapper.OperatorGraphMapper;
import it.units.malelab.jgea.core.representation.tree.*;
import it.units.malelab.jgea.core.selector.Last;
import it.units.malelab.jgea.core.selector.Tournament;
import it.units.malelab.jgea.core.solver.*;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.problem.symbolicregression.*;
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
import static it.units.malelab.jgea.sample.Args.i;
import static it.units.malelab.jgea.sample.Args.ri;

public class RegressionComparison extends Worker {

  public RegressionComparison(String[] args) {
    super(args);
  }

  public static void main(String[] args) {
    new RegressionComparison(args);
  }

  @Override
  public void run() {
    final SymbolicRegressionFitness.Metric metric = SymbolicRegressionFitness.Metric.MSE;

    // Trees' structuralRules
    List<String> treeBaseRules;
    try (Stream<String> treeRulesPath = Files.lines(Paths.get("./prolog/trees/structuralRules.txt"))) {
      treeBaseRules = treeRulesPath.collect(Collectors.toList());
      treeBaseRules.add(0, "max_const(10.0).");
      treeBaseRules.add(0, "min_const(0.1).");
    } catch (IOException e) {
      throw new UnsupportedOperationException("Tree's structural rules not found in given path");
    }
    // Ffnn's structuralRules
    List<String> ffnnBaseRules;
    try (Stream<String> ffnnRulesPath = Files.lines(Paths.get("./prolog/ffnn/structuralRules.txt"))) {
      ffnnBaseRules = ffnnRulesPath.collect(Collectors.toList());
      ffnnBaseRules.add(0, "max_weight(5.0).");
      ffnnBaseRules.add(0, "min_weight(-5.0).");
      ffnnBaseRules.add(0, "n_output(1).");
      ffnnBaseRules.add(0, "max_size(141).");

    } catch (IOException e) {
      throw new UnsupportedOperationException("Ffnn's structural rules not found in given path");
    }

    List<String> treeRulesInput1 = new ArrayList<>(treeBaseRules);
    treeRulesInput1.add(0, "n_input(1).");
    List<String> ffnnRulesInput1 = new ArrayList<>(ffnnBaseRules);
    ffnnRulesInput1.add(0, "n_input(1).");

    List<SyntheticSymbolicRegressionProblem> problemsInput1 = List.of(
            new Polynomial2(metric),
            new Polynomial4(metric),
            new Nguyen7(metric, 1),
            new Keijzer6(metric)
    );
    runSameDomain(treeRulesInput1, ffnnRulesInput1, problemsInput1, "Ffnn-PolyNguyKeij.csv");

    List<String> treeRulesInput5 = new ArrayList<>(treeBaseRules);
    treeRulesInput5.add(0, "n_input(5).");
    List<String> ffnnRulesInput5 = new ArrayList<>(ffnnBaseRules);
    ffnnRulesInput5.add(0, "n_input(5).");
    runSameDomain(treeRulesInput5, ffnnRulesInput5, Arrays.asList(new Vladislavleva4(metric, 1)), "Ffnn-Vladislav.csv");


    List<String> treeRulesInput2 = new ArrayList<>(treeBaseRules);
    treeRulesInput2.add(0, "n_input(2).");
    List<String> ffnnRulesInput2 = new ArrayList<>(ffnnBaseRules);
    ffnnRulesInput2.add(0, "n_input(2).");
    runSameDomain(treeRulesInput2, ffnnRulesInput2, Arrays.asList(new Pagie1(metric)), "Ffnn-Pagie.csv");

  }

  private void runSameDomain(List<String> treeStructuralRules, List<String> ffnnStructuralRules, List<SyntheticSymbolicRegressionProblem> problems, String filename) {
    final int nPop = i(a("nPop", "70"));
    final int nTournament = 5;
    final int diversityMaxAttempts = 100;
    final int nIterations = i(a("nIterations", "100"));
    final int[] seeds = ri(a("seed", "0:30"));

    Element.Operator[] gpOperators = new Element.Operator[]{Element.Operator.ADDITION, Element.Operator.SUBTRACTION,
            Element.Operator.MULTIPLICATION, Element.Operator.DIVISION};
    double[] gpConstants = new double[]{0.1, 1d, 10d};

    final int minFactoryDim = 5;
    final int maxFactoryDim = 125;
    final int minTreeFactoryHeight = (int) (Math.log(minFactoryDim + 2.0 + 1.0) / Math.log(2)) - 1;
    final int maxTreeFactoryHeight = (int) (Math.log(maxFactoryDim + 2.0 + 1.0) / Math.log(2)) - 1;
    final int maxTreeHeight = i(a("maxTreeHeight", "10")); // nonProlog trees

//    // Trees
//    final PrologGraph treeOrigin = getTreeOrigin();
//    final List<String> treeDomain = Arrays.asList(
//            ":- dynamic node_id/1.",
//            ":- dynamic start/2.",
//            ":- dynamic type/2.",
//            ":- dynamic value/2.",
//            ":- dynamic edge_id/1.",
//            ":- dynamic edge/3.");
//    final String treeOperatorsPath = "./prolog/trees/operators/";
//    List<List<String>> treePrologOperatorsSelection = getLabelledOperators(treeOperatorsPath + "selection");
//    List<List<String>> treePrologOperatorsAll = getLabelledOperators(treeOperatorsPath + "others");
//    treePrologOperatorsAll.addAll(treePrologOperatorsSelection);
//    Map<GeneticOperator<PrologGraph>, Double> treeSelOperatorsMap = mapOperatorsEqualWeight(treePrologOperatorsSelection, treeDomain, treeStructuralRules);
//    Map<GeneticOperator<PrologGraph>, Double> treeAllOperatorsMap = mapOperatorsEqualWeight(treePrologOperatorsAll, treeDomain, treeStructuralRules);
//    // factories
//    List<String> treeFactoryOperatorsAll = new ArrayList<>();
//    List<String> treeFactoryOperatorsSelection = new ArrayList<>();
//    try {
//      final List<String> treeFactorySelection = Arrays.asList("addSubTree.txt"); //selection
//      for (String fileName : treeFactorySelection) {
//        treeFactoryOperatorsSelection.add(Files.readString(Path.of(treeOperatorsPath + "selection/" + fileName)));
//      }
//      final List<String> treeFactoryOthers = Arrays.asList("innerSubTree.txt", "innerSubTree.txt"); //others
//      for (String fileName : treeFactoryOthers) {
//        treeFactoryOperatorsAll.add(Files.readString(Path.of(treeOperatorsPath + "others/" + fileName)));
//      }
//      treeFactoryOperatorsAll.addAll(treeFactoryOperatorsSelection);
//    } catch (IOException any) {
//      throw new UnsupportedOperationException("IOException in trees' factories.");
//    }

    // Ffnn
    final PrologGraph ffnnOrigin = getFfnnOrigin();
    final List<String> ffnnDomain = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic layer/2.",
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


    // Consumers
    List<NamedFunction<? super POSetPopulationState<?, ?, ? extends Double>, ?>> functions = List.of(
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
            fitness().reformat("%5.3f").of(best()),
            hist(8).of(each(fitness())).of(all()),
            hist(8).of(each(size().of(genotype()))).of(all()),
            max(Comparator.comparingDouble(Number::doubleValue)).reformat("%3d").of(each(size().of(genotype()))).of(all()),
            min(Comparator.comparingDouble(Number::doubleValue)).reformat("%3d").of(each(size().of(genotype()))).of(all()),
            solution().reformat("%80.80s").of(best())
    );

    List<NamedFunction<? super Map<String, Object>, ?>> kFunctions = List.of(
            attribute("seed").reformat("%2d"),
            attribute("problem").reformat(NamedFunction.formatOfLongest(problems.stream()
                    .map(p -> p.getClass().getSimpleName())
                    .toList())),
            attribute("evolver").reformat("%20.20s")
    );
    ListenerFactory<POSetPopulationState<?, ?, ? extends Double>, Map<String, Object>> listenerFactory =
            new TabularPrinter<>(
                    functions,
                    kFunctions
            );
    listenerFactory = ListenerFactory.all(List.of(
            listenerFactory,
            new CSVPrinter<>(functions, kFunctions, new File("./prolog/results/" + filename))
    ));


    // Evolvers
    Map<String, Function<SyntheticSymbolicRegressionProblem, IterativeSolver<? extends POSetPopulationState<?,
            RealFunction,
            Double>, SyntheticSymbolicRegressionProblem, RealFunction>>> solvers = new TreeMap<>();

//    solvers.put("prolog-tree-enfdiv-all", p -> new StandardWithEnforcedDiversityEvolver<>(
//            new OperatorGraphMapper().andThen(og -> new RealFunction() {
//              @Override
//              public double apply(double... input) {
//                return og.apply(input)[0];
//              }
//
//              public String toString() {
//                return og.toString();
//              }
//            }),
//            new PrologGraphFactory(minFactoryDim, maxFactoryDim, treeOrigin, treeFactoryOperatorsAll, treeDomain, treeStructuralRules),
//            nPop,
//            StopConditions.nOfIterations(nIterations),
//            treeAllOperatorsMap,
//            new Tournament(nTournament),
//            new Last(),
//            nPop,
//            true,
//            false,
//            (srp, r) -> new POSetPopulationState<>(),
//            diversityMaxAttempts
//    ));
//
//    solvers.put("prolog-tree-enfdiv-selection", p -> new StandardWithEnforcedDiversityEvolver<>(
//            new OperatorGraphMapper().andThen(og -> new RealFunction() {
//              @Override
//              public double apply(double... input) {
//                return og.apply(input)[0];
//              }
//
//              public String toString() {
//                return og.toString();
//              }
//            }),
//            new PrologGraphFactory(minFactoryDim, maxFactoryDim, treeOrigin, treeFactoryOperatorsSelection, treeDomain, treeStructuralRules),
//            nPop,
//            StopConditions.nOfIterations(nIterations),
//            treeSelOperatorsMap,
//            new Tournament(nTournament),
//            new Last(),
//            nPop,
//            true,
//            false,
//            (srp, r) -> new POSetPopulationState<>(),
//            diversityMaxAttempts
//    ));

    solvers.put("prolog-ffnn-enfdiv-all", p -> new StandardWithEnforcedDiversityEvolver<>(
            new FunctionGraphMapper(BaseFunction.TANH).andThen(fg -> new RealFunction() {
              @Override
              public double apply(double... input) {
                return fg.apply(input)[0];
              }

              public String toString() {
                return fg.toString();
              }
            }),
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

    solvers.put("prolog-ffnn-enfdiv-selection", p -> new StandardWithEnforcedDiversityEvolver<>(
            new FunctionGraphMapper(BaseFunction.TANH).andThen(fg -> new RealFunction() {
              @Override
              public double apply(double... input) {
                return fg.apply(input)[0];
              }

              public String toString() {
                return fg.toString();
              }
            }),
            new PrologGraphFactory(minFactoryDim, maxFactoryDim, ffnnOrigin, ffnnFactoryOperatorsSelection, ffnnDomain, ffnnStructuralRules),
            nPop,
            StopConditions.nOfIterations(nIterations),
            ffnnSelOperatorsMap,
            new Tournament(nTournament),
            new Last(),
            nPop,
            true,
            false,
            (srp, r) -> new POSetPopulationState<>(),
            diversityMaxAttempts
    ));

//    solvers.put("tree-gadiv", p -> {
//      IndependentFactory<Element> terminalFactory = IndependentFactory.oneOf(
//              IndependentFactory.picker(Arrays.stream(
//                              vars(p.qualityFunction().arity()))
//                      .sequential()
//                      .map(Element.Variable::new)
//                      .toArray(Element.Variable[]::new)),
//              IndependentFactory.picker(Arrays.stream(gpConstants)
//                      .mapToObj(Element.Constant::new)
//                      .toArray(Element.Constant[]::new))
//      );
//      return new StandardWithEnforcedDiversityEvolver<>(
//              ((Function<Tree<Element>, RealFunction>) t -> new TreeBasedRealFunction(
//                      t,
//                      vars(p.qualityFunction().arity())
//              )).andThen(MathUtils.linearScaler(p.qualityFunction())),
//              new RampedHalfAndHalf<>(
//                      minTreeFactoryHeight,
//                      maxTreeFactoryHeight,
//                      Element.Operator.arityFunction(),
//                      IndependentFactory.picker(gpOperators),
//                      terminalFactory
//              ),
//              nPop,
//              StopConditions.nOfIterations(nIterations),
//              Map.of(
//                      new SubtreeCrossover<>(maxTreeHeight),
//                      0.8d,
//                      new SubtreeMutation<>(
//                              maxTreeHeight,
//                              new GrowTreeBuilder<>(
//                                      Element.Operator.arityFunction(),
//                                      IndependentFactory.picker(gpOperators),
//                                      terminalFactory
//                              )
//                      ),
//                      0.2d
//              ),
//              new Tournament(nTournament),
//              new Last(),
//              nPop,
//              true,
//              false,
//              (srp, r) -> new POSetPopulationState<>(),
//              diversityMaxAttempts
//      );
//    });


    L.info(String.format("Going to test with %d evolvers: %s%n", solvers.size(), solvers.keySet()));
    //run
    for (int seed : seeds) {
      for (SyntheticSymbolicRegressionProblem problem : problems) {
        for (Map.Entry<String, Function<SyntheticSymbolicRegressionProblem, IterativeSolver<?
                extends POSetPopulationState<?,
                RealFunction, Double>, SyntheticSymbolicRegressionProblem, RealFunction>>> solverEntry :
                solvers.entrySet()) {
          Map<String, Object> keys = Map.ofEntries(
                  Map.entry("seed", seed),
                  Map.entry("problem", problem.getClass().getSimpleName().toLowerCase()),
                  Map.entry("evolver", solverEntry.getKey())
          );
          try {
            Stopwatch stopwatch = Stopwatch.createStarted();
            IterativeSolver<? extends POSetPopulationState<?, RealFunction, Double>, SyntheticSymbolicRegressionProblem,
                    RealFunction> solver = solverEntry.getValue()
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

  private PrologGraph getTreeOrigin() {
    PrologGraph tree = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("start", 1);
    node1.put("type", "operator");
    node1.put("value", "+");
    LinkedHashMap<String, Object> node2 = new LinkedHashMap<>();
    node2.put("node_id", "second");
    node2.put("start", 0);
    node2.put("type", "constant");
    node2.put("value", 1d);
    LinkedHashMap<String, Object> node3 = new LinkedHashMap<>();
    node3.put("node_id", "third");
    node3.put("start", 0);
    node3.put("type", "input");
    node3.put("value", 0);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "firstEdge");
    LinkedHashMap<String, Object> edge2 = new LinkedHashMap<>();
    edge2.put("edge_id", "secondEdge");
    tree.addNode(node1);
    tree.addNode(node2);
    tree.addNode(node3);
    tree.setArcValue(node2, node1, edge1);
    tree.setArcValue(node3, node1, edge2);
    return tree;
  }

  private PrologGraph getFfnnOrigin() {
    PrologGraph ffnn = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("layer", 0);
    LinkedHashMap<String, Object> node2 = new LinkedHashMap<>();
    node2.put("node_id", "second");
    node2.put("layer", 1);
    LinkedHashMap<String, Object> node3 = new LinkedHashMap<>();
    node3.put("node_id", "third");
    node3.put("layer", 2);
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
    return ffnn;
  }

  private static String[] vars(int n) {
    if (n == 1) {
      return new String[]{"x"};
    }
    String[] vars = new String[n];
    for (int i = 0; i < n; i++) {
      vars[i] = "x" + i;
    }
    return vars;
  }
}
