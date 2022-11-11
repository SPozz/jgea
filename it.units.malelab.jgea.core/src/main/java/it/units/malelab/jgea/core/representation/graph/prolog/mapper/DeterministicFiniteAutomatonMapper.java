package it.units.malelab.jgea.core.representation.graph.prolog.mapper;

import it.units.malelab.jgea.core.representation.graph.Graph;
import it.units.malelab.jgea.core.representation.graph.GraphUtils;
import it.units.malelab.jgea.core.representation.graph.LinkedHashGraph;
import it.units.malelab.jgea.core.representation.graph.finiteautomata.DeterministicFiniteAutomaton;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphUtils;
import it.units.malelab.jgea.core.representation.graph.prolog.analysis.FsmAnalysis;
import org.jpl7.Query;

import java.util.*;
import java.util.function.Function;

public class DeterministicFiniteAutomatonMapper {//implements Function<PrologGraph,DeterministicFiniteAutomaton<Character>> {

  //  @Override
  public static DeterministicFiniteAutomaton<Character> apply(PrologGraph prologFsm) {
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
      Character input = prologFsm.getArcValue(arc).get("input").toString().charAt(0);
      Set<Character> inputSet = new HashSet<>();
      inputSet.add(input);
      intermediateGraph.setArcValue(source, target, inputSet);
    }

    return new DeterministicFiniteAutomaton<>(intermediateGraph);
  }

  public static void main(String[] args) {
    // Domain:
    List<String> domainDefinition = Arrays.asList(
            ":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic accepting/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic input/2.");

    List<Object> inputSymbols = Arrays.asList('0', '1');

//    PrologGraph fsm = FsmAnalysis.generateFSMGraph(100, domainDefinition, inputSymbols);

    // TESTING

    List<String> graphDescription = Arrays.asList(
            "node_id(a)",
            "node_id(b)",
            "node_id(c)",
            "start(a,1)",
            "start(b,0)",
            "start(c,0)",
            "accepting(a,0)",
            "accepting(b,0)",
            "accepting(c,1)",
            "edge_id(a0)",
            "edge_id(a1)",
            "edge_id(b0)",
            "edge_id(b1)",
            "edge_id(c0)",
            "edge_id(c1)",
            "edge(a,b,a1)",
            "edge(a,a,a0)",
            "edge(b,c,b1)",
            "edge(b,a,b0)",
            "edge(c,c,c0)",
            "edge(c,c,c1)",
            "input(a0,0)",
            "input(b0,0)",
            "input(c0,0)",
            "input(a1,1)",
            "input(b1,1)",
            "input(c1,1)"
    );


    for (String fact : graphDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    PrologGraph example = PrologGraphUtils.buildGraph(domainDefinition);


    DeterministicFiniteAutomaton<Character> resultingGraph = apply(example);
    System.out.println("Result: " + resultingGraph);

    List<Character> match1 = Arrays.asList('1', '0','1','1','0');
    List<Character> match2 = Arrays.asList('1', '1');
    List<Character> match3 = Arrays.asList('0', '0');
    System.out.println(resultingGraph.match(match1));
    System.out.println(resultingGraph.match(match2));
    System.out.println(resultingGraph.match(match3));


    System.out.println("\n"+resultingGraph.extract(match1));

  }
}
