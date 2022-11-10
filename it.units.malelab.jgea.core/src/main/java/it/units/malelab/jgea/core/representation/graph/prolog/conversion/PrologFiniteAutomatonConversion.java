package it.units.malelab.jgea.core.representation.graph.prolog.conversion;

import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.finiteautomata.DeterministicFiniteAutomaton;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.analysis.FsmAnalysis;

import java.util.*;

public class PrologFiniteAutomatonConversion {

  public static DeterministicFiniteAutomaton<String> convert(PrologGraph prologFsm) {
    LinkedHashGraph<DeterministicFiniteAutomaton.State, Set<String>> intermediateGraph = new LinkedHashGraph<>();
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
      String input = prologFsm.getArcValue(arc).get("input").toString();
      Set<String> inputSet = new HashSet<>();
      inputSet.add(input);
      intermediateGraph.setArcValue(source, target, inputSet);
    }

    return new DeterministicFiniteAutomaton<>(intermediateGraph);
  }

  public static void main(String[] args) {
    // Domain:
    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");

    List<Object> inputSymbols = Arrays.asList(0, 1);

    PrologGraph fsm = FsmAnalysis.generateFSMGraph(100, domainDefinition, inputSymbols);

    // Testing
    LinkedHashMap<String, Object> sourceNode = new LinkedHashMap<>();
    sourceNode.put("node_id", "test111");
    sourceNode.put("accepting", 0);
    sourceNode.put("start", 0);
    LinkedHashMap<String, Object> targetNode = new LinkedHashMap<>();
    targetNode.put("node_id", "test222");
    targetNode.put("accepting", 1);
    targetNode.put("start", 0);

    fsm.addNode(sourceNode);
    fsm.addNode(targetNode);

    LinkedHashMap<String, Object> value = new LinkedHashMap<>();
    value.put("edge_id", "doubleEdge");
    value.put("input", 999);
    fsm.setArcValue(sourceNode, targetNode, value);
    value.remove("input", 999);
    value.put("input", 333);
    fsm.setArcValue(sourceNode, targetNode, value);

    System.out.println(fsm.arcs());

    DeterministicFiniteAutomaton<String> resultingGraph = convert(fsm);

    System.out.println("Result: " + resultingGraph);


  }
}
