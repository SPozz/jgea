package it.units.malelab.jgea.sample.lab.prolog;

import com.google.common.base.Stopwatch;
import it.units.malelab.jgea.core.IndependentFactory;
import it.units.malelab.jgea.core.listener.CSVPrinter;
import it.units.malelab.jgea.core.listener.ListenerFactory;
import it.units.malelab.jgea.core.listener.NamedFunction;
import it.units.malelab.jgea.core.listener.TabularPrinter;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.representation.graph.numeric.RealFunction;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologOperator;
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

public class TreeRegressionComparison extends Worker {

  public TreeRegressionComparison(String[] args) {
    super(args);
  }

  public static void main(String[] args) {
    new TreeRegressionComparison(args);
  }


  @Override
  public void run() {
    final int nPop = i(a("nPop", "100"));
    final int maxHeight = i(a("maxHeight", "10"));
    final int nTournament = 5;
    final int diversityMaxAttempts = 100;
    final int nIterations = i(a("nIterations", "100"));
    final int[] seeds = ri(a("seed", "0:10"));
    Element.Operator[] gadivOperators = new Element.Operator[]{Element.Operator.ADDITION, Element.Operator.SUBTRACTION,
            Element.Operator.MULTIPLICATION, Element.Operator.PROT_DIVISION};
    double[] constants = new double[]{0.1, 1d, 10d};
    final int minDim = 5;
    final int maxDim = 34;
    final SymbolicRegressionFitness.Metric metric = SymbolicRegressionFitness.Metric.MSE;

    final PrologGraph originGraph = getOrigin();
    final List<String> domainDefinition = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");

    final String path = "C:\\Users\\Simone\\Desktop\\GitHub_Tesi\\jgea\\prolog\\trees\\operators\\";
    final File folderFactory = new File(path + "factory");
    final File folderAllOperators = new File(path + "others");
    final File folderSelectionOperators = new File(path + "selection");
    final List<String> factoryFilesAll = Arrays.asList("addSubTree.txt", "innerSubTree.txt", "innerSubTree.txt");
    final List<String> factoryFilesSel = Arrays.asList("addSubTree.txt");
    List<List<String>> prologOperatorsAll = new ArrayList<>();
    List<List<String>> prologOperatorSelection = new ArrayList<>();
    List<String> factoryOperatorsAll = new ArrayList<>();
    List<String> factoryOperatorsSelection = new ArrayList<>();
    List<String> structuralRules;
    try {
      // structuralRules
      Stream<String> rulesPath = Files.lines(Paths.get("C:\\Users\\Simone\\Desktop\\GitHub_Tesi\\jgea\\prolog\\trees\\structuralRules.txt"));
      structuralRules = rulesPath.collect(Collectors.toList());
      // Selection operators
      for (File file : folderSelectionOperators.listFiles()) {
        String operator = Files.readString(file.toPath());
        prologOperatorSelection.add(Arrays.asList(file.getName().replace(".txt", ""), operator));
      }
      // All operators
      for (File file : folderAllOperators.listFiles()) {
        String operator = Files.readString(file.toPath());
        prologOperatorsAll.add(Arrays.asList(file.getName().replace(".txt", ""), operator));
      }
      prologOperatorsAll.addAll(prologOperatorSelection);
      // factoryALL
      for (String fileName : factoryFilesAll) {
        factoryOperatorsAll.add(Files.readString(Path.of(folderFactory + "\\" + fileName)));
      }
      for (String fileName : factoryFilesSel) {
        factoryOperatorsSelection.add(Files.readString(Path.of(folderFactory + "\\" + fileName)));
      }
    } catch (IOException any) {
      throw new UnsupportedOperationException("IOException in main.");
    }

    Map<GeneticOperator<PrologGraph>, Double> prologSelOperatorsMap = new HashMap<>();
    final double weightSel = 1.0d / prologOperatorSelection.size();
    for (List<String> op : prologOperatorSelection)
      prologSelOperatorsMap.put(new PrologOperator(op.get(0), op.get(1), domainDefinition, structuralRules), weightSel);

    Map<GeneticOperator<PrologGraph>, Double> prologAllOperatorsMap = new HashMap<>();
    final double weight = 1.0d / prologOperatorsAll.size();
    for (List<String> op : prologOperatorsAll)
      prologAllOperatorsMap.put(new PrologOperator(op.get(0), op.get(1), domainDefinition, structuralRules), weight);

    List<SyntheticSymbolicRegressionProblem> problems = List.of(
            new Nguyen7(metric, 1),
            new Polynomial2(metric),
            new Polynomial3(metric),
            new Polynomial4(metric),
            new Keijzer6(metric)
    );

    //consumers
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
            solution().reformat("%100.100s").of(best())
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
            new CSVPrinter<>(functions, kFunctions, new File("C:\\Users\\Simone\\Desktop\\GitHub_Tesi\\jgea_data\\Evolution\\Trees\\Complete.csv"))
    ));


    // evolvers
    Map<String, Function<SyntheticSymbolicRegressionProblem, IterativeSolver<? extends POSetPopulationState<?,
            RealFunction,
            Double>, SyntheticSymbolicRegressionProblem, RealFunction>>> solvers = new TreeMap<>();

    solvers.put("prolog-enfdiv-all", p -> new StandardWithEnforcedDiversityEvolver<>(
            new OperatorGraphMapper().andThen(og -> new RealFunction() {
              @Override
              public double apply(double... input) {
                return og.apply(input)[0];
              }

              public String toString() {
                return og.toString();
              }
            }),
            new PrologGraphFactory(minDim, maxDim, originGraph, factoryOperatorsAll, domainDefinition, structuralRules),
            nPop,
            StopConditions.nOfIterations(nIterations),
            prologAllOperatorsMap,
            new Tournament(nTournament),
            new Last(),
            nPop,
            true,
            false,
            (srp, r) -> new POSetPopulationState<>(),
            diversityMaxAttempts
    ));

    solvers.put("prolog-enfdiv-sel", p -> new StandardWithEnforcedDiversityEvolver<>(
            new OperatorGraphMapper().andThen(og -> new RealFunction() {
              @Override
              public double apply(double... input) {
                return og.apply(input)[0];
              }

              public String toString() {
                return og.toString();
              }
            }),
            new PrologGraphFactory(minDim, maxDim, originGraph, factoryOperatorsSelection, domainDefinition, structuralRules),
            nPop,
            StopConditions.nOfIterations(nIterations),
            prologSelOperatorsMap,
            new Tournament(nTournament),
            new Last(),
            nPop,
            true,
            false,
            (srp, r) -> new POSetPopulationState<>(),
            diversityMaxAttempts
    ));

    //benchmark
    solvers.put("tree-gadiv", p -> {
      IndependentFactory<Element> terminalFactory = IndependentFactory.oneOf(
              IndependentFactory.picker(Arrays.stream(
                              vars(p.qualityFunction().arity()))
                      .sequential()
                      .map(Element.Variable::new)
                      .toArray(Element.Variable[]::new)),
              IndependentFactory.picker(Arrays.stream(constants)
                      .mapToObj(Element.Constant::new)
                      .toArray(Element.Constant[]::new))
      );
      return new StandardWithEnforcedDiversityEvolver<>(
              ((Function<Tree<Element>, RealFunction>) t -> new TreeBasedRealFunction(
                      t,
                      vars(p.qualityFunction().arity())
              )).andThen(MathUtils.linearScaler(p.qualityFunction())),
              new RampedHalfAndHalf<>(
                      4,
                      maxHeight,
                      Element.Operator.arityFunction(),
                      IndependentFactory.picker(gadivOperators),
                      terminalFactory
              ),
              nPop,
              StopConditions.nOfIterations(nIterations),
              Map.of(
                      new SubtreeCrossover<>(maxHeight),
                      0.8d,
                      new SubtreeMutation<>(
                              maxHeight,
                              new GrowTreeBuilder<>(
                                      Element.Operator.arityFunction(),
                                      IndependentFactory.picker(gadivOperators),
                                      terminalFactory
                              )
                      ),
                      0.2d
              ),
              new Tournament(nTournament),
              new Last(),
              nPop,
              true,
              false,
              (srp, r) -> new POSetPopulationState<>(),
              diversityMaxAttempts
      );
    });


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

  private PrologGraph getOrigin() {
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
    return origin;
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
