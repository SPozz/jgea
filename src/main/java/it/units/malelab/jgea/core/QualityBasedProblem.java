package it.units.malelab.jgea.core;

import it.units.malelab.jgea.core.order.PartialComparator;

import java.util.function.Function;

public interface QualityBasedProblem<S, Q> extends Problem<S> {

  PartialComparator<Q> qualityComparator();

  Function<S, Q> qualityFunction();

  @Override
  default PartialComparatorOutcome compare(S s1, S s2) {
    return qualityComparator().compare(qualityFunction().apply(s1), qualityFunction().apply(s2));
  }

  default QualityBasedProblem<S, Q> withComparator(PartialComparator<Q> comparator) {
    QualityBasedProblem<S, Q> inner = this;
    return new QualityBasedProblem<>() {
      @Override
      public PartialComparator<Q> qualityComparator() {
        return comparator;
      }

      @Override
      public Function<S, Q> qualityFunction() {
        return inner.qualityFunction();
      }
    };
  }
}
