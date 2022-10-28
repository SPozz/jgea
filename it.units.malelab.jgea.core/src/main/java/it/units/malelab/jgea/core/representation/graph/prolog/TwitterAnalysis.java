package it.units.malelab.jgea.core.representation.graph.prolog;

import org.jpl7.Query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class TwitterAnalysis {

  static PrologGraph generateGraph(int dimension, List<String> domainDefinition, List<String> structuralRules) {
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
        throw new RuntimeException("something wrong");
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

    List<String> edgeIDs = new ArrayList<>();
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
        --j;
        continue;
      }
      source = sourceType.get(random.nextInt(0, sourceType.size()));
      target = targetType.get(random.nextInt(0, targetType.size()));
      edgeID = source + target;
      if (source.equals(target)) {
        --j;
        continue;
      }
      if (edgeIDs.contains(edgeID)) {
        --j;
        continue;
      }
      edgeIDs.add(edgeID);
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

    for (String fact : graphDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    for (String rule : structuralRules) {
      rule = rule.replace(".", "");
      rule = rule.replace(" ", "");
      Query.hasSolution("assert((" + rule + "))");
    }

    return PrologGraphUtils.buildGraph(domainDefinition);
  }

  public static void main(String[] args) {

    List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic type/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic action/2.");

    List<String> structuralRules = Arrays.asList("action_value(retweet).",
            "action_value(post).",
            "action_value(cite).",
            "action_value(follows).",
            "type_value(user).",
            "type_value(tweet).",
            "cite_check(X) :- action(X,cite),edge(S,T,X), type(S,tweet), type(T,user).",
            "post_check(X) :- action(X,post),edge(S,T,X), type(S,user), type(T,tweet).",
            "retweet_check(X) :- action(X,retweet),edge(S,T,X), type(S,user), type(T,tweet).",
            "follows_check(X) :- action(X,follows),edge(S,T,X), type(S,user), type(T,user), S \\== T.",
            "size([], 0).",
            "size([_|Xs], N) :- size(Xs, N1), N is N1 + 1.",
            "tweet_indeg(T) :- findall(S, (edge(S,T,X),action(X,post)), S), size(S,N1), N1 == 1.",
            "is_valid :- foreach(findall(E,(edge_id(E),action(E,cite)),E), maplist(cite_check,E))," +
                    "    foreach(findall(E,(edge_id(E),action(E,post)),E), maplist(post_check,E))," +
                    "    foreach(findall(E,(edge_id(E),action(E,retweet)),E), maplist(retweet_check,E))," +
                    "    foreach(findall(E,(edge_id(E),action(E,follows)),E), maplist(follows_check,E))," +
                    "    foreach(findall(T,type(T,tweet),T), maplist(tweet_indeg,T))."
    );

    generateGraph(40, domainDefinition, structuralRules);
  }
}
