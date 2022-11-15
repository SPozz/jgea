package it.units.malelab.jgea.core.representation.graph.prolog.mapper;

import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.finiteautomata.DeterministicFiniteAutomaton;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;


import java.util.*;
import java.util.function.Function;

public class DeterministicFiniteAutomatonMapper implements Function<PrologGraph, DeterministicFiniteAutomaton<Character>> {

  //  @Override
  public DeterministicFiniteAutomaton<Character> apply(PrologGraph prologFsm) {
    LinkedHashGraph<DeterministicFiniteAutomaton.State, Set<Character>> intermediateGraph = new LinkedHashGraph<>();
    int index = 1; //index 0 reserved for start node
    LinkedHashMap<String, DeterministicFiniteAutomaton.State> idToState = new LinkedHashMap<>();

    for (Map<String, Object> node : prologFsm.nodes()) {
      DeterministicFiniteAutomaton.State tmpNode;
      boolean accepting = (node.get("accepting").toString().equals("1"));

      if (node.get("start").toString().equals("1")) {
        tmpNode = new DeterministicFiniteAutomaton.State(0, accepting);
      } else {
        tmpNode = new DeterministicFiniteAutomaton.State(index, accepting);
        index++;
      }

      idToState.put(node.get("node_id").toString(), tmpNode);
      intermediateGraph.addNode(tmpNode);
    }

    for (Graph.Arc<Map<String, Object>> arc : prologFsm.arcs()) {
      DeterministicFiniteAutomaton.State source = idToState.get((String) arc.getSource().get("node_id"));
      DeterministicFiniteAutomaton.State target = idToState.get((String) arc.getTarget().get("node_id"));
      String inputString = prologFsm.getArcValue(arc).get("input").toString();
      inputString = inputString.substring(1, inputString.length() - 1); //exclude brackets
      inputString = inputString.replace(" ", "");

      Set<Character> inputSet = new HashSet<>();
      while (inputString.contains(",")) {
        inputSet.add(inputString.charAt(0));
        inputString = inputString.substring(2);
      }
      inputSet.add(inputString.charAt(0));
      intermediateGraph.setArcValue(source, target, inputSet);
    }

    return new DeterministicFiniteAutomaton<>(intermediateGraph);
  }


}
