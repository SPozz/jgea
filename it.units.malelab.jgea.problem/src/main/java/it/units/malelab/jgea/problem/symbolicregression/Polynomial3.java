package it.units.malelab.jgea.problem.symbolicregression;

public class Polynomial3 extends SyntheticSymbolicRegressionProblem {

  public Polynomial3(SymbolicRegressionFitness.Metric metric) {
    super(
            v -> {
              double x = v[0];
              return x * x * x + 1.5d * x * x - 0.5d * x + 0.7d;
            },
            MathUtils.pairwise(MathUtils.equispacedValues(-1, 1, .05)),
            MathUtils.pairwise(MathUtils.equispacedValues(-1, 1, .05)),
            metric
    );
  }
}
