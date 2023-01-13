package it.units.malelab.jgea.core.solver;

import it.units.malelab.jgea.core.Factory;
import it.units.malelab.jgea.core.QualityBasedProblem;
import it.units.malelab.jgea.core.operator.GeneticOperator;
import it.units.malelab.jgea.core.order.PartialComparator;
import it.units.malelab.jgea.core.order.PartiallyOrderedCollection;
import it.units.malelab.jgea.core.selector.Selector;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.core.util.Misc;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.random.RandomGenerator;

public class AdaptiveEvolver<P extends QualityBasedProblem<S, Q>, G, S, Q> extends StandardWithEnforcedDiversityEvolver<AdaptiveEvolver.State<G, S, Q>, P, G, S, Q> {


  public static class State<G, S, Q> extends POSetPopulationState<G, S, Q> {
    private final Map<GeneticOperator<G>, Double> operators;

    public State(Map<GeneticOperator<G>, Double> operators) {
      this.operators = new LinkedHashMap<>(operators);
    }

    public State(LocalDateTime startingDateTime, long elapsedMillis, long nOfIterations, long nOfBirths, long nOfFitnessEvaluations, PartiallyOrderedCollection<Individual<G, S, Q>> population, Map<GeneticOperator<G>, Double> operators) {
      super(startingDateTime, elapsedMillis, nOfIterations, nOfBirths, nOfFitnessEvaluations, population);
      this.operators = operators;
    }

    @Override
    public POSetPopulationState<G, S, Q> immutableCopy() {
      return new State<>(
              startingDateTime,
              elapsedMillis,
              nOfIterations,
              nOfBirths,
              nOfFitnessEvaluations,
              population.immutableCopy(),
              new LinkedHashMap<>(operators)
      );
    }

    public Map<GeneticOperator<G>, Double> getOperators() {
      return operators;
    }
  }

  private record OperatorApplication<G>(List<G> parentGenotypes, List<G> childGenotypes) {
  }

//  Map<GeneticOperator<G>, Double> operatorProbabilities;

  private final Function<Long, Double> probabilityVariationSchedule;

  public AdaptiveEvolver(
          Function<? super G, ? extends S> solutionMapper,
          Factory<? extends G> genotypeFactory,
          int populationSize,
          Predicate<? super AdaptiveEvolver.State<G, S, Q>> stopCondition,
          Map<GeneticOperator<G>, Double> operators,
          Selector<? super Individual<? super G, ? super S, ? super Q>> parentSelector,
          Selector<? super Individual<? super G, ? super S, ? super Q>> unsurvivalSelector,
          int offspringSize,
          boolean overlapping,
          boolean remap,
          int maxAttempts,
          Function<Long, Double> perturbFunction
  ) {
    super(solutionMapper, genotypeFactory, populationSize, stopCondition, operators, parentSelector, unsurvivalSelector, offspringSize, overlapping, remap,
            (p, r) -> new State<>(operators), maxAttempts
    );
//    this.operatorProbabilities = initialProbability;
    this.probabilityVariationSchedule = perturbFunction;
  }

  @Override
  protected Collection<Individual<G, S, Q>> buildOffspring(State<G, S, Q> state, P problem, RandomGenerator random, ExecutorService executor) throws SolverException {
    Collection<G> offspringGenotypes = new ArrayList<>();
    Collection<G> existingGenotypes = state.getPopulation().all().stream().map(Individual::genotype).toList();
    Map<GeneticOperator<G>, List<OperatorApplication<G>>> operatorApplications = new LinkedHashMap<>();
    state.operators.keySet().forEach(op -> operatorApplications.put(op, new ArrayList<>()));
    while (offspringGenotypes.size() < offspringSize) {
      GeneticOperator<G> operator = Misc.pickRandomly(state.operators, random);
      List<G> parentGenotypes = new ArrayList<>(operator.arity());
      int attempts = 0;
      while (true) {
        parentGenotypes.clear();
        for (int j = 0; j < operator.arity(); j++) {
          Individual<G, S, Q> parent = parentSelector.select(state.getPopulation(), random);
          parentGenotypes.add(parent.genotype());
        }
        List<G> childGenotypes = new ArrayList<>(operator.apply(parentGenotypes, random));
        boolean added = false;
        for (G childGenotype : childGenotypes) {
          if ((!offspringGenotypes.contains(childGenotype) && !existingGenotypes.contains(childGenotype)) || (attempts >= super.maxAttempts - 1)) {
            added = true;
            offspringGenotypes.add(childGenotype);
            operatorApplications.get(operator).add(
                    new OperatorApplication<>(parentGenotypes, childGenotypes)
            );
          }
        }
        if (added) {
          break;
        }
        attempts = attempts + 1;
      }
    }
    List<Individual<G, S, Q>> offspring = map(offspringGenotypes, List.of(), solutionMapper, problem.qualityFunction(), executor, state);
    //update operators map
    List<Map.Entry<GeneticOperator<G>, Double>> changes = operatorApplications.entrySet().stream()
            .map(e -> Map.entry(
                    e.getKey(),
                    computeEffectiveness(problem, e.getValue(), state.getPopulation().all(), offspring)
            ))
            .toList();

    // given effectivenesses, apply change (ev. anche moltiplicando per 1+ effectivenees se valore con segno)
    //TODO: check and then reformat
    long iteration = state.getNOfIterations();
    double epsilon = Math.min(Math.abs(probabilityVariationSchedule.apply(iteration)), 1d);

    for (Map.Entry<GeneticOperator<G>, Double> operatorChange : changes) {
      GeneticOperator<G> operator = operatorChange.getKey();
      double oldValue = state.getOperators().get(operator);
      double effectiveness = operatorChange.getValue();
      state.getOperators().put(operator, (1.0d + (effectiveness > 0 ? 1d : -1d) * epsilon) * oldValue);
    }
    return offspring;
  }

  private double computeEffectiveness(P problem, List<OperatorApplication<G>> applications, Collection<Individual<G, S, Q>> parents, Collection<Individual<G, S, Q>> offspring) {
    // obtain list of comparison outcome
    Map<G, Q> genotypeQualities = new IdentityHashMap<>();
    parents.forEach(i -> genotypeQualities.put(i.genotype(), i.fitness()));
    offspring.forEach(i -> genotypeQualities.put(i.genotype(), i.fitness()));
    return applications.stream()
            .mapToDouble(oa -> effectiveness(outcomes(
                    oa.parentGenotypes().stream().map(genotypeQualities::get).toList(),
                    oa.childGenotypes().stream().map(genotypeQualities::get).toList(),
                    problem.qualityComparator()
            )))
            .average().orElse(0d);
  }

  private static <Q> List<PartialComparator.PartialComparatorOutcome> outcomes(List<Q> qParents, List<Q> qChildren, PartialComparator<Q> comparator) {
    // prodotto cartesiano
    // metti tutti i risultati
    // TODO: check
    List<PartialComparator.PartialComparatorOutcome> comparatorOutcomes = new ArrayList<>();
    for (Q qParent : qParents) {
      for (Q qChild : qChildren) {
        comparatorOutcomes.add(comparator.compare(qParent, qChild));
      }
    }
    return comparatorOutcomes;
  }

  private static double effectiveness(List<PartialComparator.PartialComparatorOutcome> outcomes) {
    // tanti before, male, tanti after, bene. Normalizzare (e.g. risultato tra -1 e 1)
    // trascura gli altri(?)

    // Nota: non sto trascurando gli altri, nel senso che sono considerati nella normalizzazione
    //TODO: check
    int nBefore = 0;
    int nAfter = 0;
    for (PartialComparator.PartialComparatorOutcome outcome : outcomes) {
      if (outcome.equals(PartialComparator.PartialComparatorOutcome.BEFORE)) {
        nBefore++;
      } else if (outcome.equals(PartialComparator.PartialComparatorOutcome.AFTER)) {
        nAfter++;
      }
    }
    return ((double) (nAfter - nBefore)) / (outcomes.size());
  }
}
