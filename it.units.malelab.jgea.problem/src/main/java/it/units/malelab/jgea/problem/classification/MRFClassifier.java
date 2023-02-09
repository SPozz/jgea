package it.units.malelab.jgea.problem.classification;

import java.util.function.Function;

public class MRFClassifier implements Classifier<double[], Integer> {
  private final Function<double[], double[]> function;

  public MRFClassifier(Function<double[], double[]> function) {
    this.function = function;
  }

  @Override
  public Label<Integer> classify(double[] inputs) {
    double[] outputs = function.apply(inputs);
    int argMax = 0;
    for (int i = 0; i < outputs.length; ++i) {
      argMax = outputs[i] > outputs[argMax] ? i : argMax;
    }
    Label.IntLabelFactory labelFactory = new Label.IntLabelFactory(outputs.length);
    return labelFactory.getLabel(argMax);
  }
}