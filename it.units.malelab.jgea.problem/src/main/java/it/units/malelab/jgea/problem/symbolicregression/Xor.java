package it.units.malelab.jgea.problem.symbolicregression;

import java.util.List;

public class Xor extends SyntheticSymbolicRegressionProblem {

  public Xor(SymbolicRegressionFitness.Metric metric) {
    super(
            v -> {
              double x = v[0];
              double y = v[1];
              if (x == y) {
                return 1;
              }
              return -1;
            },
            List.of(
                    new double[]{-1d, 1d},
                    new double[]{1d, 1d},
                    new double[]{1d, -1d},
                    new double[]{-1d, -1d}
            ),
            List.of(
                    new double[]{-1d, 1d},
                    new double[]{1d, 1d},
                    new double[]{1d, -1d},
                    new double[]{-1d, -1d}
            ),
            metric
    );
  }
}
