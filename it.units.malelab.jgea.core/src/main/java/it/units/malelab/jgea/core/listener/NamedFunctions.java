package it.units.malelab.jgea.core.listener;

import it.units.malelab.jgea.core.solver.AdaptiveEvolver;
import it.units.malelab.jgea.core.solver.Individual;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.core.solver.state.State;
import it.units.malelab.jgea.core.util.Misc;
import it.units.malelab.jgea.core.util.Pair;
import it.units.malelab.jgea.core.util.Sized;
import it.units.malelab.jgea.core.util.TextPlotter;

import java.util.*;
import java.util.function.Function;

/**
 * @author eric on 2021/01/02 for jgea
 */
public class NamedFunctions {

  private final static String DEFAULT_FORMAT = "%s";
  private final static long CACHE_SIZE = 100;

  private NamedFunctions() {
  }

  public static <G, S, F> NamedFunction<POSetPopulationState<? extends G, ? extends S, ? extends F>, Collection<?
          extends Individual<? extends G, ? extends S, ? extends F>>> all() {
    return f("all", e -> e.getPopulation().all());
  }

  @SuppressWarnings("unchecked")
  public static <T> NamedFunction<Object, T> as(Class<T> clazz) {
    return f("as[" + clazz.getSimpleName() + "]", o -> (T) o);
  }

  public static <V> NamedFunction<Map<String, V>, V> attribute(String name) {
    return f(name, "%" + name.length() + "s", map -> map.get(name));
  }

  public static <V> List<NamedFunction<? super Map<String, V>, V>> attributes(String... names) {
    List<NamedFunction<? super Map<String, V>, V>> functions = new ArrayList<>();
    for (String name : names) {
      functions.add(attribute(name));
    }
    return functions;
  }

  public static <G, S, F> NamedFunction<POSetPopulationState<? extends G, ? extends S, ? extends F>, Individual<?
          extends G, ? extends S, ? extends F>> best() {
    return f("best", e -> Misc.first(e.getPopulation().firsts()));
  }

  public static <G, S, F> NamedFunction<POSetPopulationState<? extends G, ? extends S, ? extends F>, Long> births() {
    return f("births", "%5d", POSetPopulationState::getNOfBirths);
  }

  public static <F, T> NamedFunction<F, T> cachedF(String name, Function<F, T> function) {
    return f(name, Misc.cached(function, CACHE_SIZE));
  }

  public static <F, T> NamedFunction<F, T> cachedF(String name, String format, Function<F, T> function) {
    return f(name, format, Misc.cached(function, CACHE_SIZE));
  }

  public static <F, T> NamedFunction<F, T> cachedF(String name, String format, Function<F, T> function, long size) {
    return f(name, format, Misc.cached(function, size));
  }

  public static <T> NamedFunction<State, T> constant(
          String name, String format, T value
  ) {
    return f(name, format, e -> value);
  }

  public static <T> NamedFunction<State, T> constant(
          String name, T value
  ) {
    return constant(name, NamedFunction.format(value.toString().length()), value);
  }

  public static <F, T> NamedFunction<Collection<? extends F>, Collection<T>> each(NamedFunction<F, T> mapper) {
    return f(
            "each[" + mapper.getName() + "]",
            individuals -> individuals.stream().map(mapper).collect(java.util.stream.Collectors.toList())
    );
  }

  public static NamedFunction<State, Float> elapsedSeconds() {
    return f("elapsed.seconds", "%5.1f", e -> e.getElapsedMillis() / 1000f);
  }

  public static <F, T> NamedFunction<F, T> f(String name, Function<F, T> function) {
    return f(name, DEFAULT_FORMAT, function);
  }

  public static <F, T> NamedFunction<F, T> f(String name, String format, Function<F, T> function) {
    return NamedFunction.build(name, format, function);
  }

  public static <G, S, F> NamedFunction<POSetPopulationState<? extends G, ? extends S, ? extends F>, Collection<?
          extends Individual<? extends G, ? extends S, ? extends F>>> firsts() {
    return f("firsts", e -> e.getPopulation().firsts());
  }

  public static <F> NamedFunction<Individual<?, ?, ? extends F>, F> fitness() {
    return f("fitness", Individual::fitness);
  }

  public static <G, S, F> NamedFunction<POSetPopulationState<? extends G, ? extends S, ? extends F>, Long> fitnessEvaluations() {
    return f("fitness.evaluations", "%5d", POSetPopulationState::getNOfFitnessEvaluations);
  }

  public static <G, S, F> NamedFunction<Individual<? extends G, ? extends S, ? extends F>, Long> fitnessMappingIteration() {
    return f("birth.iteration", "%4d", Individual::fitnessMappingIteration);
  }

  public static <G> NamedFunction<Individual<? extends G, ?, ?>, G> genotype() {
    return f("genotype", Individual::genotype);
  }

  public static <G, S, F> NamedFunction<Individual<? extends G, ? extends S, ? extends F>, Long> genotypeBirthIteration() {
    return f("genotype.birth.iteration", "%4d", Individual::genotypeBirthIteration);
  }

  @SuppressWarnings("unchecked")
  public static NamedFunction<Collection<? extends Number>, String> hist(int bins) {
    return f(
            "hist",
            NamedFunction.format(bins),
            values -> TextPlotter.histogram(values instanceof List ? (List<? extends Number>) values : new ArrayList<>(
                    values), bins)
    );
  }

  public static NamedFunction<State, Long> iterations() {
    return f("iterations", "%4d", State::getNOfIterations);
  }

  public static <G, S, F> NamedFunction<POSetPopulationState<? extends G, ? extends S, ? extends F>, Collection<?
          extends Individual<? extends G, ? extends S, ? extends F>>> lasts() {
    return f("lasts", e -> e.getPopulation().lasts());
  }

  public static <T> NamedFunction<Collection<? extends T>, T> max(Comparator<T> comparator) {
    return f("max", ts -> ts.stream().max(comparator).orElse(null));
  }

  public static <T> NamedFunction<Collection<? extends T>, T> median(Comparator<T> comparator) {
    return f("median", ts -> Misc.median(ts, comparator));
  }

  public static <T> NamedFunction<Collection<? extends T>, T> min(Comparator<T> comparator) {
    return f("min", ts -> ts.stream().min(comparator).orElse(null));
  }

  public static <T> NamedFunction<List<? extends T>, T> nth(int index) {
    return f("[" + index + "]", l -> l.get(index));
  }

  public static <T> NamedFunction<Collection<? extends T>, T> one() {
    return f("one", Misc::first);
  }

  public static NamedFunction<Object, Number> size() {
    return f("size", "%3d", NamedFunctions::size);
  }

  public static Integer size(Object o) {
    if (o instanceof Sized s) {
      return s.size();
    }
    if (o instanceof Collection<?> c) {
      if (Misc.first(c) instanceof Sized s) {
        return c.stream().mapToInt(i -> s.size()).sum();
      }
      return c.size();
    }
    if (o instanceof String s) {
      return s.length();
    }
    if (o instanceof Pair p) {
      Integer firstSize = size(p.first());
      Integer secondSize = size(p.second());
      if ((firstSize != null) && (secondSize != null)) {
        return firstSize + secondSize;
      }
    }
    return null;
  }

  public static <S> NamedFunction<Individual<?, ? extends S, ?>, S> solution() {
    return f("solution", Individual::solution);
  }

  public static NamedFunction<Collection<?>, Double> uniqueness() {
    return f("uniqueness", "%4.2f", ts -> (double) ts.stream().distinct().count() / (double) ts.size());
  }

  public static NamedFunction<POSetPopulationState<?, ?, ?>, String> operatorsProbabilitiesPlot(int n) {
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

  public static NamedFunction<POSetPopulationState<?, ?, ?>, Double> operatorProbability(int n) {
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

  public static NamedFunction<POSetPopulationState<?, ?, ?>, List<Map.Entry<String, Double>>> operatorsProbabilitiesAll() {
    return f(
            "all-operators-probabilities",
            (POSetPopulationState<?, ?, ?> s) -> {
              if (s instanceof AdaptiveEvolver.State<?, ?, ?> as) {
                return as.getOperators().entrySet().stream()
                        .map(e -> Map.entry(e.getKey().toString(), e.getValue()))
                        .sorted(Map.Entry.comparingByKey(String::compareTo))
                        .toList();
              }
              return null;
            });
  }

}
