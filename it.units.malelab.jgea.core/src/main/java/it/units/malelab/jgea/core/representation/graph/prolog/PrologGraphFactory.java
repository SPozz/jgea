package it.units.malelab.jgea.core.representation.graph.prolog;

import it.units.malelab.jgea.core.Factory;

import java.util.List;
import java.util.random.RandomGenerator;

public class PrologGraphFactory implements Factory<PrologGraph> {

  final private int minDimension;
  final private int maxDimension;
  final private PrologGraph originGraph;
  final private List<String> operators;
  final private List<String> domainDefinition;
  final private List<String> structuralRules;


  PrologGraphFactory(int minDimension, int maxDimension, PrologGraph originGraph, List<String> operators, List<String> domainDefinition, List<String> structuralRules) {
    this.minDimension = minDimension;
    this.maxDimension = maxDimension;
    this.originGraph = originGraph;
    this.domainDefinition = domainDefinition;
    this.structuralRules = structuralRules;
    this.operators = operators;
  }

  @Override
  public List<PrologGraph> build(int n, RandomGenerator random) {
    throw new UnsupportedOperationException("TODO");
  }

}
