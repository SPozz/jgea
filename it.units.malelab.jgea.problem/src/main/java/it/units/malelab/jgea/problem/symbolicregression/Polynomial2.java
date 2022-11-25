package it.units.malelab.jgea.problem.symbolicregression;

public class Polynomial2 extends SyntheticSymbolicRegressionProblem {


    public Polynomial2 (SymbolicRegressionFitness.Metric metric){
      super(
              v -> {
                double x = v[0];
                return x * x + x + 1;
              },
              MathUtils.pairwise(MathUtils.equispacedValues(-1, 1, .01)),
              MathUtils.pairwise(MathUtils.equispacedValues(-1, 1, .01)),
              metric
      );
    }


}
