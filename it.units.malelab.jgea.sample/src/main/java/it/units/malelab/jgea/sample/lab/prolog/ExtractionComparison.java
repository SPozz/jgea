/*
 * Copyright 2020 Eric Medvet <eric.medvet@gmail.com> (as eric)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package it.units.malelab.jgea.sample.lab.prolog;

import com.google.common.base.Stopwatch;
import com.google.common.collect.Sets;
import it.units.malelab.jgea.core.QualityBasedProblem;
import it.units.malelab.jgea.core.distance.Jaccard;
import it.units.malelab.jgea.core.listener.CSVPrinter;
import it.units.malelab.jgea.core.listener.ListenerFactory;
import it.units.malelab.jgea.core.listener.NamedFunction;
import it.units.malelab.jgea.core.listener.TabularPrinter;
import it.units.malelab.jgea.core.operator.Crossover;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.operator.Mutation;
import it.units.malelab.jgea.core.order.LexicoGraphical;
import it.units.malelab.jgea.core.representation.grammar.cfggp.GrammarBasedSubtreeMutation;
import it.units.malelab.jgea.core.representation.grammar.cfggp.GrammarRampedHalfAndHalf;
import it.units.malelab.jgea.core.representation.graph.*;
import it.units.malelab.jgea.core.representation.graph.finiteautomata.DeterministicFiniteAutomaton;
import it.units.malelab.jgea.core.representation.graph.finiteautomata.Extractor;
import it.units.malelab.jgea.core.representation.graph.finiteautomata.ShallowDFAFactory;
import it.units.malelab.jgea.core.representation.graph.numeric.RealFunction;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphFactory;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologOperator;
import it.units.malelab.jgea.core.representation.graph.prolog.mapper.DeterministicFiniteAutomatonMapper;
import it.units.malelab.jgea.core.representation.tree.SameRootSubtreeCrossover;
import it.units.malelab.jgea.core.representation.tree.Tree;
import it.units.malelab.jgea.core.selector.Last;
import it.units.malelab.jgea.core.selector.Tournament;
import it.units.malelab.jgea.core.solver.*;
import it.units.malelab.jgea.core.solver.speciation.LazySpeciator;
import it.units.malelab.jgea.core.solver.speciation.SpeciatedEvolver;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.core.util.Misc;
import it.units.malelab.jgea.core.util.Pair;
import it.units.malelab.jgea.problem.extraction.ExtractionFitness;
import it.units.malelab.jgea.problem.extraction.string.RegexBasedExtractor;
import it.units.malelab.jgea.problem.extraction.string.RegexExtractionProblem;
import it.units.malelab.jgea.problem.extraction.string.RegexGrammar;
import it.units.malelab.jgea.problem.symbolicregression.Element;
import it.units.malelab.jgea.sample.Worker;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static it.units.malelab.jgea.core.listener.NamedFunctions.*;
import static it.units.malelab.jgea.sample.Args.i;
import static it.units.malelab.jgea.sample.Args.ri;

/**
 * @author eric
 */
public class ExtractionComparison extends Worker {

  public ExtractionComparison(String[] args) {
    super(args);
  }

  public static void main(String[] args) {
    new ExtractionComparison(args);
  }

  public void run() {
    final int nPop = i(a("nPop", "70"));
    final int nTournament = 5;
    final int diversityMaxAttempts = 100;
    final int nIterations = i(a("nIterations", "100"));
    final int[] seeds = ri(a("seed", "0:30"));

    final int minFactoryDim = 5;
    final int maxFactoryDim = 125;

    ExtractionFitness.Metric[] metrics = new ExtractionFitness.Metric[]{ExtractionFitness.Metric.SYMBOL_WEIGHTED_ERROR};
    Map<String, RegexExtractionProblem> problems = Map.ofEntries(
            Map.entry(
                    "synthetic-2-5",
                    RegexExtractionProblem.varAlphabet(2, 5, 1, metrics)
            ),
            Map.entry("synthetic-3-5", RegexExtractionProblem.varAlphabet(3, 5, 1, metrics)),
            Map.entry("synthetic-4-8", RegexExtractionProblem.varAlphabet(4, 8, 1, metrics)),
            Map.entry("synthetic-4-10", RegexExtractionProblem.varAlphabet(4, 10, 1, metrics))
    ).entrySet().stream().map(e -> Pair.of(e.getKey(), e.getValue())).collect(Collectors.toMap(
            Pair::first,
            Pair::second
    ));


    final PrologGraph fsmOrigin = getFsmOrigin();
    final List<String> fsmDomainDefinition = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");

    List<String> fsmStructuralRules;
    try (Stream<String> fsmRulesPath = Files.lines(Paths.get("./prolog/fsm/structuralRules.txt"))) {
      fsmStructuralRules = fsmRulesPath.collect(Collectors.toList());
      final int nSymbols = 2;
      for (int i = 0; i < nSymbols; ++i) {
        fsmStructuralRules.add(0, "input_val(" + i + ").");
      }

    } catch (IOException e) {
      throw new UnsupportedOperationException("Fsm structural rules not found in given path");
    }


    final String fsmOperatorsPath = "./prolog/fsm/operators/";
    List<List<String>> fsmPrologOperatorsSelection = getLabelledOperators(fsmOperatorsPath + "selection");
    List<List<String>> fsmPrologOperatorsAll = getLabelledOperators(fsmOperatorsPath + "others");
    fsmPrologOperatorsAll.addAll(fsmPrologOperatorsSelection);
    Map<GeneticOperator<PrologGraph>, Double> fsmSelOperatorsMap = mapOperatorsEqualWeight(fsmPrologOperatorsSelection, fsmDomainDefinition, fsmStructuralRules);
    Map<GeneticOperator<PrologGraph>, Double> fsmAllOperatorsMap = mapOperatorsEqualWeight(fsmPrologOperatorsAll, fsmDomainDefinition, fsmStructuralRules);
    // factories
    List<String> fsmFactoryOperatorsAll = new ArrayList<>();
    List<String> fsmFactoryOperatorsSelection = new ArrayList<>();
    try {
      final List<String> fsmFactorySelection = Arrays.asList("addEdge.txt", "addFinalLayer.txt", "addInitialLayer.txt", "addNode.txt"); //TODO: choose selection operators
      for (String fileName : fsmFactorySelection) {
        fsmFactoryOperatorsSelection.add(Files.readString(Path.of(fsmOperatorsPath + "selection/" + fileName)));
      }
      final List<String> fsmFactoryOthers = Arrays.asList("addConnectedNode.txt", "addNodeAndEdge.txt"); //TODO: choose factory others
      for (String fileName : fsmFactoryOthers) {
        fsmFactoryOperatorsAll.add(Files.readString(Path.of(fsmOperatorsPath + "others/" + fileName)));
      }
      fsmFactoryOperatorsAll.addAll(fsmFactoryOperatorsSelection);
    } catch (IOException any) {
      throw new UnsupportedOperationException("IOException in fsm factories creation.");
    }

    //consumers
    Map<String, Object> keys = new HashMap<>();
    List<NamedFunction<? super POSetPopulationState<?, ? extends Extractor<Character>, ? extends List<Double>>, ?>> functions = List.of(
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
            size().of(solution()).of(best()),
            nth(0).reformat("%5.3f").of(fitness()).of(best()),
            fitnessMappingIteration().of(best()),
            solution().reformat("%30.30s").of(best())
    );
    List<NamedFunction<? super Map<String, Object>, ?>> kFunctions = List.of(
            attribute("seed").reformat("%2d"),
            attribute("problem").reformat("%20.20s"),
            attribute("evolver").reformat("%20.20s")
    );
    ListenerFactory<POSetPopulationState<?, ? extends Extractor<Character>, ? extends List<Double>>, Map<String,
            Object>> listenerFactory = new TabularPrinter<>(
            functions,
            kFunctions
    );
    if (a("file", null) != null) {
      listenerFactory = ListenerFactory.all(List.of(
              listenerFactory,
              new CSVPrinter<>(functions, kFunctions, new File("./prolog/results/FSM-extraction.csv"))
      ));
    }
    //evolvers
    Map<String, Function<RegexExtractionProblem, IterativeSolver<? extends POSetPopulationState<?,
            Extractor<Character>, List<Double>>, QualityBasedProblem<Extractor<Character>, List<Double>>,
            Extractor<Character>>>> solvers = new TreeMap<>();

//    solvers.put("prolog-fsm-enfdiv-all", p -> new StandardWithEnforcedDiversityEvolver<>(
//            new DeterministicFiniteAutomatonMapper().andThen(),//TODO: function here
//            new PrologGraphFactory(minFactoryDim, maxFactoryDim, fsmOrigin, fsmFactoryOperatorsAll, fsmDomainDefinition, fsmStructuralRules),
//            nPop,
//            StopConditions.nOfIterations(nIterations),
//            fsmAllOperatorsMap,
//            new Tournament(nTournament),
//            new Last(),
//            nPop,
//            true,
//            false,
//            (srp, rnd) -> new POSetPopulationState<>(),
//            diversityMaxAttempts
//    ));
//
//    solvers.put("prolog-fsm-enfdiv-selection", p -> new StandardWithEnforcedDiversityEvolver<>(
//            new DeterministicFiniteAutomatonMapper().andThen(),//TODO: function here
//            new PrologGraphFactory(minFactoryDim, maxFactoryDim, fsmOrigin, fsmFactoryOperatorsSelection, fsmDomainDefinition, fsmStructuralRules),
//            nPop,
//            StopConditions.nOfIterations(nIterations),
//            fsmSelOperatorsMap,
//            new Tournament(nTournament),
//            new Last(),
//            nPop,
//            true,
//            false,
//            (srp, rnd) -> new POSetPopulationState<>(),
//            diversityMaxAttempts
//    ));

    //run
    for (int seed : seeds) {
      for (Map.Entry<String, RegexExtractionProblem> problemEntry : problems.entrySet()) {
        for (Map.Entry<String, Function<RegexExtractionProblem, IterativeSolver<? extends POSetPopulationState<?,
                Extractor<Character>, List<Double>>, QualityBasedProblem<Extractor<Character>, List<Double>>,
                Extractor<Character>>>> solverEntry : solvers.entrySet()) {
          keys.putAll(Map.ofEntries(
                  Map.entry("seed", seed),
                  Map.entry("problem", problemEntry.getKey()),
                  Map.entry("evolver", solverEntry.getKey())
          ));
          try {
            RegexExtractionProblem p = problemEntry.getValue();
            Stopwatch stopwatch = Stopwatch.createStarted();
            IterativeSolver<? extends POSetPopulationState<?, Extractor<Character>, List<Double>>,
                    QualityBasedProblem<Extractor<Character>, List<Double>>, Extractor<Character>> solver =
                    solverEntry.getValue()
                            .apply(p);
            L.info(String.format("Starting %s", keys));
            Collection<Extractor<Character>> solutions =
                    solver.solve(
                            p.withComparator(new LexicoGraphical<>(
                                    Double.class,
                                    IntStream.range(0, metrics.length).toArray()
                            )),
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


  private PrologGraph getFsmOrigin() {
    PrologGraph fsm = new PrologGraph();
    LinkedHashMap<String, Object> node1 = new LinkedHashMap<>();
    node1.put("node_id", "first");
    node1.put("start", 1);
    node1.put("accepting", 1);
    LinkedHashMap<String, Object> edge1 = new LinkedHashMap<>();
    edge1.put("edge_id", "loopEdge");
    edge1.put("input", "[0,1]");
    fsm.addNode(node1);
    fsm.setArcValue(node1, node1, edge1);
    return fsm;
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
}
