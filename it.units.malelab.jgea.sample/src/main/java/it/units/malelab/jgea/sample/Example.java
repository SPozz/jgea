/*
 * Copyright 2022 eric
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

package it.units.malelab.jgea.sample;

import it.units.malelab.jgea.core.QualityBasedProblem;
import it.units.malelab.jgea.core.listener.ListenerFactory;
import it.units.malelab.jgea.core.listener.NamedFunction;
import it.units.malelab.jgea.core.listener.TabularPrinter;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.representation.sequence.UniformCrossover;
import it.units.malelab.jgea.core.representation.sequence.bit.*;
import it.units.malelab.jgea.core.selector.Last;
import it.units.malelab.jgea.core.selector.Tournament;
import it.units.malelab.jgea.core.solver.*;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.core.util.Misc;
import it.units.malelab.jgea.core.util.TextPlotter;
import it.units.malelab.jgea.problem.synthetic.OneMax;

import java.io.FileNotFoundException;
import java.util.*;
import java.util.function.Function;

import static it.units.malelab.jgea.core.listener.NamedFunctions.*;

public class Example extends Worker {

  public final static List<NamedFunction<? super POSetPopulationState<?, ?, ?>, ?>> BASIC_FUNCTIONS =
          List.of(
                  iterations(),
                  births(),
                  elapsedSeconds(),
                  operatorsProbabilitiesPlot(4),
                  operatorProbability(0),
                  operatorProbability(1),
                  operatorProbability(2),
                  operatorProbability(3),
                  size().of(all()),
                  size().of(firsts()),
                  size().of(lasts()),
                  uniqueness().of(each(genotype())).of(all()),
                  uniqueness().of(each(solution())).of(all()),
                  uniqueness().of(each(fitness())).of(all()),
                  size().of(genotype()).of(best()),
                  size().of(solution()).of(best()),
                  fitnessMappingIteration().of(best())
          );

  public final static List<NamedFunction<? super POSetPopulationState<?, ?, ? extends Double>, ?>> DOUBLE_FUNCTIONS =
          List.of(
                  fitness().reformat("%5.3f").of(best()),
                  hist(8).of(each(fitness())).of(all()),
                  max(Double::compare).reformat("%5.3f").of(each(fitness())).of(all())
          );

  public Example(String[] args) {
    super(args);
  }

  public static void main(String[] args) throws FileNotFoundException {
    new Example(args);
  }

  @Override
  public void run() {
    runOneMax();
//    String problem = a("problem", "oneMax");
//    if (problem.equals("oneMax")) {
//      runOneMax();
//    }
//    if (problem.equals("symbolicRegression")) {
//      runSymbolicRegression();
//    }
//    if (problem.equals("ackley")) {
//      runAckley();
//    }
  }

  public void runOneMax() {
    int size = 1000;
    Random r = new Random(1);
    QualityBasedProblem<BitString, Double> p = new OneMax();
    List<NamedFunction<? super POSetPopulationState<?, ?, ?>, ?>> keysFunctions = List.of();
    ListenerFactory<POSetPopulationState<?, ?, ? extends Double>, Map<String, Object>> listenerFactory =
            ListenerFactory.all(
                    List.of(new TabularPrinter<>(
                            Misc.concat(List.of(keysFunctions, BASIC_FUNCTIONS, DOUBLE_FUNCTIONS)),
                            List.of(attribute("solver"))
                    )));
    List<IterativeSolver<? extends POSetPopulationState<?, BitString, Double>, QualityBasedProblem<BitString, Double>
            , BitString>> solvers = new ArrayList<>();


//    solvers.add(new RandomSearch<>(
//            Function.identity(),
//            new BitStringFactory(size),
//            StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(100))
//    ));
//    solvers.add(new RandomWalk<>(
//        Function.identity(),
//        new BitStringFactory(size),
//        StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(100)),
//        new BitFlipMutation(0.01d)
//    ));
//    solvers.add(new StandardEvolver<POSetPopulationState<BitString, BitString, Double>, QualityBasedProblem<BitString
//            , Double>, BitString, BitString, Double>(
//            Function.identity(),
//            new BitStringFactory(size),
//            100,
//            StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(100)),
//            Map.of(new UniformCrossover<>(new BitStringFactory(size)), 0.8d, new BitFlipMutation(0.01d), 0.2d),
//            new Tournament(5),
//            new Last(),
//            100,
//            true,
//            false,
//            (problem, random) -> new POSetPopulationState<>()
//    ));

    int maxIterations = 100;
    double epsilon = 0.025d;
    Function<Long, Double> constantPerturbationHigh = x -> 0.025d;
    Function<Long, Double> constantPerturbationLow = x -> 0.001d;
    Function<Long, Double> constantPerturbationStepInit = x -> x<maxIterations/2?0.01:0;
    Function<Long, Double> constantPerturbationStepEnd = x -> x>maxIterations/2?0.01:0;

    solvers.add(new StandardWithEnforcedDiversityEvolver<POSetPopulationState<BitString, BitString, Double>,
            QualityBasedProblem<BitString, Double>, BitString, BitString, Double>(
            Function.identity(),
            new BitStringFactory(size),
            100,
            StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(maxIterations)),
            Map.of(new UniformCrossover<>(new BitStringFactory(size)), 0.8d, new BitFlipMutation(0.01d), 0.2d),
            new Tournament(5),
            new Last(),
            100,
            true,
            false,
            (problem, random) -> new POSetPopulationState<>(),
            100
    ));


    Map<GeneticOperator<BitString>, Double> operatorsProbBase = new HashMap<>();
    operatorsProbBase.put(new UniformCrossover<>(new BitStringFactory(size)), 0.5d);
    operatorsProbBase.put(new BitFlipMutation(0.01d), 0.5d);

////    solvers.add(
//    AdaptiveEvolver adaptiveEvolver2 = new AdaptiveEvolver<>(
//
//            Function.identity(),
//            new BitStringFactory(size),
//            100,
//            StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(maxIterations)),
//            operatorsProbBase,
//            new Tournament(5),
//            new Last(),
//            100,
//            true,
//            false,
//            100,
//            constantPerturbationHigh);
//    solvers.add(adaptiveEvolver2);
//
//    Map<GeneticOperator<BitString>, Double> operatorsProb3 = new HashMap<>();
//    operatorsProb3.put(new UniformCrossover<>(new BitStringFactory(size)), 0.33d);
//    operatorsProb3.put(new BitFlipMutation(0.01d), 0.33d);
//    operatorsProb3.put(new BadMutation(), 0.33d);
//    AdaptiveEvolver adaptiveEvolver3 = new AdaptiveEvolver<>(
//            Function.identity(),
//            new BitStringFactory(size),
//            100,
//            StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(maxIterations)),
//            operatorsProb3,
//            new Tournament(5),
//            new Last(),
//            100,
//            true,
//            false,
//            100,
//            constantPerturbationHigh
//    );
//    solvers.add(adaptiveEvolver3);


    Map<GeneticOperator<BitString>, Double> operatorsProb4 = new HashMap<>();
    operatorsProb4.put(new UniformCrossover<>(new BitStringFactory(size)), 0.25d);
    operatorsProb4.put(new BitFlipMutation(0.01d), 0.25d);
    operatorsProb4.put(new BadMutation(), 0.25d);
    operatorsProb4.put(new GoodMutation(), 0.25d);

//    AdaptiveEvolver adaptiveEvolver4 =
//            new AdaptiveEvolver<>(
//                    Function.identity(),
//                    new BitStringFactory(size),
//                    100,
//                    StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(maxIterations)),
//                    Map.ofEntries(
////                            Map.entry(new UniformCrossover<>(new BitStringFactory(size)), 1d),
//                            Map.entry(new BitFlipMutation(0.01d), 1d),
//                            Map.entry(new BadMutation(), 1d),
//                            Map.entry(new GoodMutation(), 1d)
//                    ),
//                    new Tournament(5),
//                    new Last(),
//                    100,
//                    true,
//                    false,
//                    100,
//                    constantPerturbationHigh
//            );
//    solvers.add(adaptiveEvolver4);

    for (IterativeSolver<? extends POSetPopulationState<?, BitString, Double>, QualityBasedProblem<BitString, Double>
            , BitString> evolver : solvers) {
      try {
        Collection<BitString> solutions = evolver.solve(
                p,
                r,
                executorService,
                listenerFactory.build(Map.of("solver", evolver.getClass().getSimpleName())).deferred(executorService)
        );
        System.out.printf("Found %d solutions with " + "%s.%n", solutions.size(), evolver.getClass().getSimpleName());
      } catch (SolverException e) {
        e.printStackTrace();
      }
    }


    listenerFactory.shutdown();
  }

  private static NamedFunction<POSetPopulationState<?, ?, ?>, String> operatorsProbabilitiesPlot(int n) {
    return NamedFunction.build(
            "operators.probabilities",
            "%" + n + "." + n + "s",
            (POSetPopulationState<?, ?, ?> s) -> {
              if (s instanceof AdaptiveEvolver.State<?, ?, ?> as) {
                return TextPlotter.barplot(as.getOperators().values().stream().limit(n).toList());
              }
              return "";
            }
    );
  }

  private static NamedFunction<POSetPopulationState<?, ?, ?>, Double> operatorProbability(int n) {
    return NamedFunction.build(
            "operator.%d.probability".formatted(n),
            "%5.3f",
            (POSetPopulationState<?, ?, ?> s) -> {
              if (s instanceof AdaptiveEvolver.State<?, ?, ?> as) {
                if (as.getOperators().size() > n) {
                  return as.getOperators().values().stream().toList().get(n);
                }
                return null;
              }
              return null;
            }
    );
  }

}
