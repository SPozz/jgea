package it.units.malelab.jgea.sample.lab.prolog;

import java.util.Comparator;

public class OperatorsComparator<GeneticOperator> implements Comparator<GeneticOperator> {

  @Override
  public int compare(GeneticOperator o1, GeneticOperator o2) {
    return o1.getClass().getSimpleName().compareTo(o2.getClass().getSimpleName());
  }
}
