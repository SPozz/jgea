package it.units.malelab.jgea.core.representation.graph.prolog;

import it.units.malelab.jgea.core.Factory;
import it.units.malelab.jgea.core.util.Misc;

import java.util.*;
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
    List<PrologGraph> graphList = new ArrayList<>();
    final int maxTries = 100;

    for (int i = 0; i < n; ++i) {
      final int dimension = random.nextInt(minDimension, maxDimension + 1);
      PrologGraph graph = originGraph;
      int nTries = 0;
      while (graph.size() < dimension && nTries < maxTries) { // Less or Different?
        String operator = Misc.pickRandomly(operators, random);
        graph = PrologGraphUtils.applyOperator(operator, graph, domainDefinition, structuralRules);
        nTries++;
      }
      graphList.add(graph);
    }
    return graphList;
  }


}
