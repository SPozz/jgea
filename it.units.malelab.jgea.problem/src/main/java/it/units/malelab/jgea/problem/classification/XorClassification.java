package it.units.malelab.jgea.problem.classification;

import it.units.malelab.jgea.core.util.Pair;

import java.util.List;

public class XorClassification extends DatasetClassificationProblem{
  public XorClassification(List<Pair<double[], Label<Integer>>> data, int folds, int i, ClassificationFitness.Metric learningErrorMetric, ClassificationFitness.Metric validationErrorMetric) {
    super(data, folds, i, learningErrorMetric, validationErrorMetric);
  }
}