/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package it.units.malelab.jgea;

import com.google.common.collect.Lists;
import it.units.malelab.jgea.core.Individual;
import it.units.malelab.jgea.core.Node;
import it.units.malelab.jgea.core.evolver.DeterministicCrowdingEvolver;
import it.units.malelab.jgea.core.evolver.StandardEvolver;
import it.units.malelab.jgea.core.evolver.stopcondition.FitnessEvaluations;
import it.units.malelab.jgea.core.evolver.stopcondition.PerfectFitness;
import it.units.malelab.jgea.core.fitness.Linearization;
import it.units.malelab.jgea.core.function.Function;
import it.units.malelab.jgea.core.genotype.BitString;
import it.units.malelab.jgea.core.genotype.BitStringFactory;
import it.units.malelab.jgea.core.listener.Listener;
import it.units.malelab.jgea.core.listener.collector.Basic;
import it.units.malelab.jgea.core.listener.collector.BestInfo;
import it.units.malelab.jgea.core.listener.collector.BestPrinter;
import it.units.malelab.jgea.core.listener.collector.Diversity;
import it.units.malelab.jgea.core.listener.collector.FunctionOfBest;
import it.units.malelab.jgea.core.listener.collector.Population;
import it.units.malelab.jgea.core.operator.BitFlipMutation;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.operator.LenghtPreservingTwoPointCrossover;
import it.units.malelab.jgea.core.ranker.ComparableRanker;
import it.units.malelab.jgea.core.ranker.FitnessComparator;
import it.units.malelab.jgea.core.ranker.selector.Tournament;
import it.units.malelab.jgea.core.ranker.selector.Worst;
import it.units.malelab.jgea.core.util.Misc;
import it.units.malelab.jgea.core.util.Pair;
import it.units.malelab.jgea.distance.Distance;
import it.units.malelab.jgea.distance.Edit;
import it.units.malelab.jgea.distance.Pairwise;
import it.units.malelab.jgea.distance.StringSequence;
import it.units.malelab.jgea.distance.TreeLeaves;
import it.units.malelab.jgea.grammarbased.cfggp.RampedHalfAndHalf;
import it.units.malelab.jgea.grammarbased.cfggp.StandardTreeCrossover;
import it.units.malelab.jgea.grammarbased.cfggp.StandardTreeMutation;
import it.units.malelab.jgea.problem.booleanfunction.EvenParity;
import it.units.malelab.jgea.problem.mapper.EnhancedProblem;
import it.units.malelab.jgea.problem.mapper.FitnessFunction;
import it.units.malelab.jgea.problem.mapper.MapperGeneration;
import it.units.malelab.jgea.problem.mapper.RecursiveMapper;
import it.units.malelab.jgea.problem.mapper.element.Element;
import it.units.malelab.jgea.problem.symbolicregression.Pagie1;
import it.units.malelab.jgea.problem.synthetic.KLandscapes;
import it.units.malelab.jgea.problem.synthetic.Text;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 *
 * @author eric
 */
public class RepresentationEvolution extends Worker {

  private final static Logger L = Logger.getLogger(RepresentationEvolution.class.getName());
  private final static long CACHE_SIZE = 10000;

  public final static void main(String[] args) throws FileNotFoundException {
    new RepresentationEvolution(args);
  }

  public RepresentationEvolution(String[] args) throws FileNotFoundException {
    super(args);
  }

  @Override
  public void run() {
    List<EnhancedProblem> problems = new ArrayList<>();
    try {
      problems.add(new EnhancedProblem<>(new EvenParity(3), (Distance) (new Pairwise<>(new TreeLeaves<>(new Edit<>()))).cached(CACHE_SIZE), null));
      problems.add(new EnhancedProblem<>(new Pagie1(), (Distance) (new TreeLeaves<>(new Edit<>()).cached(CACHE_SIZE)), null));
      problems.add(new EnhancedProblem<>(new KLandscapes(5), (Distance) (new TreeLeaves<>(new Edit<>()).cached(CACHE_SIZE)), null));
      problems.add(new EnhancedProblem<>(new Text("Hello World!"), new StringSequence(new Edit<>()).cached(CACHE_SIZE), null));
    } catch (IOException ex) {
      L.log(Level.SEVERE, "Cannot instantiate problems", ex);
      System.exit(-1);
    }
    int learningGenotypeSize = 64;
    int learningN = 16;
    int learningMaxMappingDepth = 9;
    int learningRuns = 1;
    int learningFitnessEvaluations = 2000;
    int learningPopulation = 500;
    int learningDepth = 14;
    int validationGenotypeSize = 128;
    int validationN = 400;
    int validationMaxMappingDepth = 12;
    int validationRuns = 2;
    int validationFitnessEvaluations = 10000;
    int validationPopulation = 250;
    List<FitnessFunction.Property> properties = Arrays.asList(
            FitnessFunction.Property.DEGENERACY,
            FitnessFunction.Property.NON_LOCALITY,
            FitnessFunction.Property.NON_UNIFORMITY);
    //iterate
    for (EnhancedProblem problem : problems) {
      List<EnhancedProblem> learningProblems = new ArrayList<>(problems);
      learningProblems.remove(problem);
      List<EnhancedProblem> validationProblems = Collections.singletonList(problem);
      for (int learningRun = 0; learningRun < learningRuns; learningRun++) {
        for (int propertiesSize = 1; propertiesSize <= properties.size(); propertiesSize++) {
          List<FitnessFunction.Property> localProperties = properties.subList(0, propertiesSize);
          try {
            //prepare problem
            MapperGeneration mapperGeneration = new MapperGeneration(
                    learningProblems, learningGenotypeSize, learningN, learningMaxMappingDepth, localProperties,
                    validationProblems, validationGenotypeSize, validationN, validationMaxMappingDepth, properties,
                    learningRun);
            Map<GeneticOperator<Node<String>>, Double> operators = new LinkedHashMap<>();
            operators.put(new StandardTreeMutation<>(learningDepth, mapperGeneration.getGrammar()), 0.2d);
            operators.put(new StandardTreeCrossover<>(learningDepth), 0.8d);
            Distance<Node<Element>> innerDistance = new TreeLeaves<>(new Edit<>());
            Distance<Individual<Node<String>, Pair<Node<Element>, Node<Element>>, List<Double>>> distance = (i1, i2, l) -> {
              double dFirst = innerDistance.apply(i1.getSolution().first(), i2.getSolution().first());
              double dSecond = innerDistance.apply(i1.getSolution().second(), i2.getSolution().second());
              return dFirst + dSecond;
            };
            double[] weights = new double[localProperties.size()];
            //prepare evolver
            Arrays.fill(weights, 1d / (double) localProperties.size());
            DeterministicCrowdingEvolver<Node<String>, Pair<Node<Element>, Node<Element>>, List<Double>> evolver = new DeterministicCrowdingEvolver<>(
                    distance,
                    learningPopulation,
                    new RampedHalfAndHalf<>(3, learningDepth, mapperGeneration.getGrammar()),
                    new ComparableRanker(new FitnessComparator<>(new Linearization(weights))),
                    mapperGeneration.getSolutionMapper(),
                    operators,
                    new Tournament<>(3),
                    new Worst<>(),
                    Lists.newArrayList(new FitnessEvaluations(learningFitnessEvaluations), new PerfectFitness<>(0d)),
                    CACHE_SIZE,
                    false
            );
            //evolve
            L.info(String.format("LEARNING\tProblems: %s\tRun: %s\tFitness: %s%n",
                    learningProblems.stream().map(p -> p.getProblem().getClass().getSimpleName()).collect(Collectors.toList()),
                    learningRun,
                    localProperties
            ));
            Random random = new Random(learningRun);
            try {
              Collection<Pair<Node<Element>, Node<Element>>> mapperPairs = evolver.solve(mapperGeneration, random, executorService,
                      Listener.onExecutor(listener(
                              new Basic(),
                              new Population(),
                              new BestInfo<>((FitnessFunction) mapperGeneration.getFitnessFunction(), "%5.3f"),
                              new FunctionOfBest("best.validation", (FitnessFunction) mapperGeneration.getValidationFunction(), 10000, "%5.3f"),
                              new Diversity(),
                              new BestPrinter()
                      ), executorService
                      ));
              Pair<Node<Element>, Node<Element>> mapperPair = Misc.first(mapperPairs);
              //iterate on problems
              for (EnhancedProblem innerProblem : problems) {
                Map<GeneticOperator<BitString>, Double> innerOperators = new LinkedHashMap<>();
                innerOperators.put(new BitFlipMutation(0.01d), 0.2d);
                innerOperators.put(new LenghtPreservingTwoPointCrossover(), 0.8d);
                for (int validationRun = 0; validationRun < validationRuns; validationRun++) {
                  RecursiveMapper recursiveMapper = new RecursiveMapper<>(
                          mapperPair.first(),
                          mapperPair.second(),
                          validationMaxMappingDepth,
                          2,
                          innerProblem.getProblem().getGrammar());
                  StandardEvolver innerEvolver = new StandardEvolver(
                          validationPopulation,
                          new BitStringFactory(validationGenotypeSize),
                          new ComparableRanker(new FitnessComparator<>(Function.identity())),
                          recursiveMapper.andThen(innerProblem.getProblem().getSolutionMapper()),
                          innerOperators,
                          new Tournament<>(3),
                          new Worst<>(),
                          validationPopulation,
                          true,
                          Lists.newArrayList(new FitnessEvaluations(validationFitnessEvaluations)),
                          CACHE_SIZE,
                          false
                  );
                  //solve validation
                  Random innerRandom = new Random(validationRun);
                  L.info(String.format("VALIDATION\tProblem: %s\tRun: %s%n",
                          innerProblem.getProblem().getClass().getSimpleName(),
                          validationRun
                  ));
                  innerEvolver.solve(innerProblem.getProblem(), innerRandom, executorService,
                          Listener.onExecutor(listener(
                                  new Basic(),
                                  new Population(),
                                  new BestInfo((Function) innerProblem.getProblem().getFitnessFunction(), "%5.3f"),
                                  new Diversity(),
                                  new BestPrinter()
                          ), executorService
                          ));
                }
              }
            } catch (InterruptedException | ExecutionException ex) {
              L.log(Level.SEVERE, String.format("Cannot solve learning run: %s", ex), ex);
              ex.printStackTrace();
            }
          } catch (IOException ex) {
            L.log(Level.SEVERE, String.format("Cannot instantiate MapperGeneration problem: %s", ex), ex);
          }
        }
      }
    }
  }

}
