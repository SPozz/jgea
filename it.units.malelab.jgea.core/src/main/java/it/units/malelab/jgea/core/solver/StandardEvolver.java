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

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package it.units.malelab.jgea.core.solver;

import it.units.malelab.jgea.core.Factory;
import it.units.malelab.jgea.core.QualityBasedProblem;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.order.DAGPartiallyOrderedCollection;
import it.units.malelab.jgea.core.order.PartiallyOrderedCollection;
import it.units.malelab.jgea.core.selector.Selector;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.core.util.Misc;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.logging.Logger;
import java.util.random.RandomGenerator;

/**
 * @author eric
 */
public class StandardEvolver<T extends POSetPopulationState<G, S, Q>, P extends QualityBasedProblem<S, Q>, G, S, Q> extends AbstractPopulationBasedIterativeSolver<T, P, G, S, Q> {

  private static final Logger L = Logger.getLogger(StandardEvolver.class.getName());
  protected final Map<GeneticOperator<G>, Double> operators;
  protected final Selector<? super Individual<? super G, ? super S, ? super Q>> parentSelector;
  protected final Selector<? super Individual<? super G, ? super S, ? super Q>> unsurvivalSelector;
  protected final int offspringSize;
  protected final boolean overlapping;
  protected final boolean remap;
  protected final BiFunction<P, RandomGenerator, T> stateInitializer;


  private final Map<GeneticOperator<G>, Integer> usage = new HashMap<>();
  private final Map<GeneticOperator<G>, Integer> changes = new HashMap<>();




  public StandardEvolver(
      Function<? super G, ? extends S> solutionMapper,
      Factory<? extends G> genotypeFactory,
      int populationSize,
      Predicate<? super T> stopCondition,
      Map<GeneticOperator<G>, Double> operators,
      Selector<? super Individual<? super G, ? super S, ? super Q>> parentSelector,
      Selector<? super Individual<? super G, ? super S, ? super Q>> unsurvivalSelector,
      int offspringSize,
      boolean overlapping,
      boolean remap,
      BiFunction<P, RandomGenerator, T> stateInitializer
  ) {
    super(solutionMapper, genotypeFactory, populationSize, stopCondition);
    this.operators = operators;
    this.parentSelector = parentSelector;
    this.unsurvivalSelector = unsurvivalSelector;
    this.offspringSize = offspringSize;
    this.overlapping = overlapping;
    this.remap = remap;
    this.stateInitializer = stateInitializer;

    for (GeneticOperator<G> op : this.operators.keySet()){
      this.changes.put(op,0);
      this.usage.put(op,0);
    }
  }

  protected Collection<Individual<G, S, Q>> buildOffspring(
      T state, P problem, RandomGenerator random, ExecutorService executor
  ) throws SolverException {




    Collection<G> offspringGenotypes = new ArrayList<>();
    while (offspringGenotypes.size() < offspringSize) {
      GeneticOperator<G> operator = Misc.pickRandomly(operators, random);

      int tmpUsage = usage.get(operator);
      usage.put(operator,++tmpUsage);

      List<G> parentGenotypes = new ArrayList<>(operator.arity());
      for (int j = 0; j < operator.arity(); j++) {
        Individual<G, S, Q> parent = parentSelector.select(state.getPopulation(), random);
        parentGenotypes.add(parent.genotype());
      }
      List<? extends G> newOffsprings = operator.apply(parentGenotypes,random);
      offspringGenotypes.addAll(newOffsprings);
//      offspringGenotypes.addAll(operator.apply(parentGenotypes, random));


      if (!newOffsprings.get(0).equals(parentGenotypes.get(0))){
        int tmpChange = changes.get(operator);
        changes.put(operator,++tmpChange);
      }


    }
    return map(offspringGenotypes, List.of(), solutionMapper, problem.qualityFunction(), executor, state);
  }

  @Override
  protected T initState(P problem, RandomGenerator random, ExecutorService executor) {
    return stateInitializer.apply(problem, random);
  }

  protected Collection<Individual<G, S, Q>> trimPopulation(
      Collection<Individual<G, S, Q>> population, P problem, RandomGenerator random
  ) {
    PartiallyOrderedCollection<Individual<G, S, Q>> orderedPopulation = new DAGPartiallyOrderedCollection<>(
        population,
        comparator(problem)
    );
    while (orderedPopulation.size() > populationSize) {
      Individual<G, S, Q> toRemoveIndividual = unsurvivalSelector.select(orderedPopulation, random);
      orderedPopulation.remove(toRemoveIndividual);
    }
    return orderedPopulation.all();
  }

  @Override
  public void update(P problem, RandomGenerator random, ExecutorService executor, T state) throws SolverException {
    Collection<Individual<G, S, Q>> offspring = buildOffspring(state, problem, random, executor);
    L.fine(String.format("Offspring built: %d individuals", offspring.size()));
    if (overlapping) {
      if (remap) {
        offspring.addAll(map(
            List.of(),
            state.getPopulation().all(),
            solutionMapper,
            problem.qualityFunction(),
            executor,
            state
        ));
      } else {
        offspring.addAll(state.getPopulation().all());
      }
      L.fine(String.format("Offspring merged with parents: %d individuals", offspring.size()));
    }
    offspring = trimPopulation(offspring, problem, random);
    L.fine(String.format("Offspring trimmed: %d individuals", offspring.size()));
    //update state
    state.setPopulation(new DAGPartiallyOrderedCollection<>(offspring, comparator(problem)));
    state.incNOfIterations();
    state.updateElapsedMillis();
  }

  public Map<GeneticOperator<G>,Integer> getUsage(){
    return usage;
  }

  public Map<GeneticOperator<G>,Integer> getChanges(){
    return changes;
  }

}
