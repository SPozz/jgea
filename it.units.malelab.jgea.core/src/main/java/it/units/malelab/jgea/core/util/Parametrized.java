package it.units.malelab.jgea.core.util;

import java.util.random.RandomGenerator;
import java.util.stream.IntStream;

/**
 * @author "Eric Medvet" on 2022/07/17 for 2dmrsim
 */
public interface Parametrized {
  double[] getParams();

  void setParams(double[] params);

  default void randomize(RandomGenerator randomGenerator, DoubleRange range) {
    setParams(IntStream.range(0, getParams().length)
            .mapToDouble(i -> randomGenerator.nextDouble(range.min(), range.max()))
            .toArray()
    );
  }
}
