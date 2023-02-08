package it.units.malelab.jgea.core.functions;

import java.util.function.BiFunction;

public interface TimedRealFunction {
  double[] apply(double t, double[] input);

  int nOfInputs();

  int nOfOutputs();

  static TimedRealFunction from(BiFunction<Double, double[], double[]> f, int nOfInputs, int nOfOutputs) {
    return new TimedRealFunction() {
      @Override
      public double[] apply(double t, double[] input) {
        if (input.length != nOfInputs) {
          throw new IllegalArgumentException(String.format(
                  "Unsupported input size: %d instead of %d",
                  input.length,
                  nOfInputs
          ));
        }
        double[] output = f.apply(t, input);
        if (output.length != nOfOutputs) {
          throw new IllegalArgumentException(String.format(
                  "Unsupported output size: %d instead of %d",
                  output.length,
                  nOfOutputs
          ));
        }
        return output;
      }

      @Override
      public int nOfInputs() {
        return nOfInputs;
      }

      @Override
      public int nOfOutputs() {
        return nOfOutputs;
      }
    };
  }

  @SuppressWarnings("unused")
  static TimedRealFunction zeros(int nOfInputs, int nOfOutputs) {
    return from((t, in) -> new double[nOfOutputs], nOfInputs, nOfOutputs);
  }

  default void checkDimension(int nOfInputs, int nOfOutputs) {
    if (nOfInputs() != nOfInputs) {
      throw new IllegalArgumentException("Wrong number of inputs: %d found, %d expected".formatted(
              nOfInputs(),
              nOfInputs
      ));
    }
    if (nOfOutputs() != nOfOutputs) {
      throw new IllegalArgumentException("Wrong number of outputs: %d found, %d expected".formatted(
              nOfOutputs(),
              nOfOutputs
      ));
    }
  }

//  @SuppressWarnings("unused")
//  default TimedRealFunction inputDiffed(double windowT, Collection<DiffInputTRF.Type> types) {
//    return new DiffInputTRF(this, windowT, types);
//  }
//
//  @SuppressWarnings("unused")
//  default TimedRealFunction outputStepped(double stepT) {
//    return new SteppedOutputTRF(this, stepT);
//  }


}
