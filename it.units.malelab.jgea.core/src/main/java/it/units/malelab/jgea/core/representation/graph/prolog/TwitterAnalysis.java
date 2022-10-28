package it.units.malelab.jgea.core.representation.graph.prolog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class TwitterAnalysis {

  static PrologGraph generateGraph(int dimension) {//List<String> domainDefinition, List<String> structuralRules
    // prendo a caso m nodi, li divido in utenti e tweet e metto in liste appropriate. Poi grafi da questi nodi
    Random random = new Random();

    List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<String> node;
    List<String> edge;
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> userIDS = new ArrayList<>();
    List<String> tweetIDS = new ArrayList<>();

    String type;
    String nodeID;
    String source;
    String target;
    String edgeID;
    String action;
    int MaxRecursion = 100;

    List<String> indexList = new ArrayList<>();

    int nNodes = random.nextInt(dimension / 4, dimension / 2);
    for (int i = 0; i < nNodes; ++i) {
      int check = 0;
      int index = random.nextInt(0, alphabet.size());
      while (indexList.contains(Integer.toString(index)) & check <= MaxRecursion) {
        index = random.nextInt(0, alphabet.size());
        ++check;
      }
      if (check == MaxRecursion) {
        continue;
      }
      indexList.add(Integer.toString(index));
      nodeID = alphabet.get(index);
      type = Arrays.asList("user", "tweet").get(random.nextInt(0, 2));
      node = Arrays.asList("node_id(" + nodeID + ")", "type(" + nodeID + "," + type + ")");
      if (type.equals("user")) {
        userIDS.add(nodeID);
      } else if (type.equals("tweet")) {
        tweetIDS.add(nodeID);
      } else
        System.out.println("ERROR, DEBUG"); // TODO: remove debugger
      allNodes.add(node);
    }

    int counter = 0;
    for (String tweet : tweetIDS) {
      source = userIDS.get(random.nextInt(0, userIDS.size()));
      target = tweet;
      edgeID = source + target;
      action = "post";
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "action(" + edgeID + "," + action + ")");
      allEdges.add(edge);
      counter += 1;
    }

    for (int j = 0; j < (dimension - nNodes - counter); ++j) {
      List<String> sourceType = Arrays.asList(userIDS, tweetIDS).get(random.nextInt(0, 2));
      List<String> targetType = Arrays.asList(userIDS, tweetIDS).get(random.nextInt(0, 2));
      if (sourceType.equals(userIDS) & targetType.equals(userIDS)) {
        action = "follows";
      } else if (sourceType.equals(tweetIDS) & targetType.equals(userIDS)) {
        action = "cite";
      } else if (sourceType.equals(userIDS) & targetType.equals(tweetIDS)) {
        action = "retweet";
      } else {
        System.out.println("DEBUG: wrong random choice"); //TODO: remove debugger
        --j;
        continue;
      }
      source = sourceType.get(random.nextInt(0, sourceType.size()));
      target = targetType.get(random.nextInt(0, targetType.size()));
      if (source.equals(target)) {
        --j;
        continue;
      }
      edgeID = source + target;
      edge = Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "action(" + edgeID + "," + action + ")");
      allEdges.add(edge);
    }

    List<String> graphDescription = new ArrayList<>();
    for (int j = 0; j < 2; ++j) {
      for (List<String> oneNode : allNodes) {
        graphDescription.add(oneNode.get(j));
      }
    }

    for (int j = 0; j < 3; ++j) {
      for (List<String> oneEdge : allEdges) {
        graphDescription.add(oneEdge.get(j));
      }
    }

    System.out.println("DEBUG, GRAPH DESCRIPTION IS:\n" + graphDescription); // TODO: remove sout

    return new PrologGraph();
  }

  public static void main(String[] args) {
    generateGraph(40);
  }
}
