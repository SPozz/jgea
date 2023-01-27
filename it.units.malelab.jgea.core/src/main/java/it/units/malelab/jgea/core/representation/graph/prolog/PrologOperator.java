package it.units.malelab.jgea.core.representation.graph.prolog;

import it.units.malelab.jgea.core.operator.Mutation;

import java.util.List;
import java.util.random.RandomGenerator;

public class PrologOperator implements Mutation<PrologGraph> {

  private final String operatorDescription;
  private final List<String> domainDefinition;
  private final List<String> structuralRules;
  private final String label;

  public PrologOperator(String label, String operatorDescription, List<String> domainDefinition, List<String> domainStructuralRules) {
    this.label = label;
    this.operatorDescription = operatorDescription;
    this.domainDefinition = domainDefinition;
    this.structuralRules = domainStructuralRules;
  }

  @Override
  public PrologGraph mutate(PrologGraph graph, RandomGenerator random) {
    return PrologGraphUtils.applyOperator(operatorDescription, graph, domainDefinition, structuralRules);
  }

  public String getLabel() {
    return this.label;
  }

  @Override
  public String toString() {
    return "PrologOperator{" +
            label +
            '}';
  }
}
