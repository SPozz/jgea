package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.Query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class TreeAnalysis {


  static PrologGraph generateGraph(int dimension, List<String> domainDefinition) {
    Random random = new Random();

    List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<String> typeValue = Arrays.asList("operand", "variable");
    List<String> operandValue = Arrays.asList("+", "*", "-", "/");

    List<String> node;
    List<String> edge;
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> edgeIDs = new ArrayList<>();

    List<String> indexList = new ArrayList<>();

    int tmpDimension = 1;

    // first node
    int start = 1;
    String type = "operand";
    String value = operandValue.get(random.nextInt(0, operandValue.size()));
    int index = random.nextInt(0, alphabet.size());
    indexList.add(Integer.toString(index));
    String nodeID = alphabet.get(index);
    node = Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")");
    allNodes.add(node);
    tmpDimension += 1;


    List<String> invalidNodes = new ArrayList<>();
    invalidNodes.add(nodeID);

    start = 0;
    int debugger = 1;
    while (!invalidNodes.isEmpty() && tmpDimension < dimension / 2) {
      String sourceID = invalidNodes.get(random.nextInt(0, invalidNodes.size()));

      // first new node
      index = random.nextInt(0, alphabet.size());
      nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debugger;
        debugger++;
      }
      indexList.add(Integer.toString(index));

      type = "operand";
      value = operandValue.get(random.nextInt(0, operandValue.size()));
      node = Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")");

      allNodes.add(node);
      tmpDimension++;
      invalidNodes.add(nodeID);

      // add edge
      String edgeID = sourceID + nodeID;
      if (edgeIDs.contains(edgeID)) {
        edgeID += debugger;
        debugger++;
      }
      edgeIDs.add(edgeID);
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + sourceID + "," + nodeID + "," + edgeID + ")");
      allEdges.add(edge);
      tmpDimension++;


      // second new node
      index = random.nextInt(0, alphabet.size());
      nodeID = alphabet.get(index);

      if (indexList.contains(Integer.toString(index))) {
        nodeID += debugger;
        debugger++;
      }
      indexList.add(Integer.toString(index));

      type = typeValue.get(random.nextInt(0, typeValue.size()));
      if (type.equals("operand")) {
        invalidNodes.add(nodeID);
        value = operandValue.get(random.nextInt(0, operandValue.size()));
      } else {
        value = Integer.toString(random.nextInt(0, 10));
      }
      node = Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + start + ")", "type(" + nodeID + "," + type + ")", "value(" + nodeID + "," + value + ")");

      allNodes.add(node);
      tmpDimension++;

      // add edge
      edgeID = sourceID + nodeID;
      if (edgeIDs.contains(edgeID)) {
        edgeID += debugger;
        debugger++;
      }
      edgeIDs.add(edgeID);
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + sourceID + "," + nodeID + "," + edgeID + ")");
      allEdges.add(edge);
      tmpDimension++;

      invalidNodes.remove(sourceID);
    }

    // convert into valid one
    while (!invalidNodes.isEmpty()) {
      nodeID = invalidNodes.get(0);
      value = Integer.toString(random.nextInt(0, 10));

      List<List<String>> addable = new ArrayList<>();
      List<List<String>> removable = new ArrayList<>();
      for (List<String> nodeList : allNodes) {
        if (nodeList.contains("node_id(" + nodeID + ")")) {
          removable.add(nodeList);
          addable.add(Arrays.asList("node_id(" + nodeID + ")", "start(" + nodeID + "," + 0 + ")", "type(" + nodeID + ",variable)", "value(" + nodeID + "," + value + ")"));
        }
      }

      for (List<String> removableElement : removable) {
        allNodes.remove(removableElement);
      }
      allNodes.addAll(addable);

      invalidNodes.remove(nodeID);
    }

    List<String> graphDescription = new ArrayList<>();
    for (int j = 0; j < 4; ++j) {
      for (List<String> oneNode : allNodes) {
        graphDescription.add(oneNode.get(j));
      }
    }

    for (int j = 0; j < 2; ++j) {
      for (List<String> oneEdge : allEdges) {
        graphDescription.add(oneEdge.get(j));
      }
    }

    for (String fact : graphDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    return PrologGraphUtils.buildGraph(domainDefinition);
  }


  public static void main(String[] args) {

    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic start/2.",
            ":- dynamic type/2.",
            ":- dynamic value/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.");
    PrologGraph graph = generateGraph(10,domainDefinition);

    System.out.println(graph.nodes());

  }
}
