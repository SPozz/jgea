package it.units.malelab.jgea.core.representation.graph.prolog.analysis;

import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraph;
import it.units.malelab.jgea.core.representation.graph.prolog.PrologGraphUtils;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.jpl7.Query;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.*;

public class TwitterAnalysis {

  public static PrologGraph generateTwitterGraph(int dimension, List<String> domainDefinition) {
    final Random random = new Random();
    final List<String> alphabet = Arrays.asList("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");
    List<List<String>> allNodes = new ArrayList<>();
    List<List<String>> allEdges = new ArrayList<>();
    List<String> userIDS = new ArrayList<>();
    List<String> tweetIDS = new ArrayList<>();

    // Attributes for graph
    final int nNodeAttributes = 2;  //ID, type
    final int nArcAttributes = 3;   //ID, edge, action
    if ((nArcAttributes + nNodeAttributes) != domainDefinition.size()) {
      throw new UnsupportedOperationException("Wrong definition of number of attributes");
    }

    List<String> indexList = new ArrayList<>();
    int debuggerID = 1;
    final int nNodes = random.nextInt(dimension / 3, dimension / 2);
    for (int i = 0; i < nNodes; ++i) {
      int index = random.nextInt(0, alphabet.size());
      String nodeID = alphabet.get(index);
      if (indexList.contains(Integer.toString(index))) {
        nodeID += debuggerID;
        debuggerID++;
      }
      indexList.add(Integer.toString(index));
      String type = Arrays.asList("user", "tweet").get(random.nextInt(0, 2));
      if (i == 0) {
        type = "user";
      } else if (i == 1) {
        type = "tweet";
      }
      if (type.equals("user")) {
        userIDS.add(nodeID);
      } else if (type.equals("tweet")) {
        tweetIDS.add(nodeID);
      } else
        throw new RuntimeException("something wrong");

      allNodes.add(Arrays.asList("node_id(" + nodeID + ")", "type(" + nodeID + "," + type + ")"));
    }

    String source;
    String target;
    String action;
    String edgeID;
    int counter = 0;
    if (!userIDS.isEmpty()) {
      for (String tweet : tweetIDS) {
        source = userIDS.get(random.nextInt(0, userIDS.size()));
        target = tweet;
        edgeID = source + target;
        action = "post";
        allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "action(" + edgeID + "," + action + ")"));
        counter += 1;
      }
    }

    final int MaxRecursion = 100;
    int recursion = 0;
    List<String> edgeIDs = new ArrayList<>();
    for (int j = 0; j < (dimension - nNodes - counter); ++j) {
      if (recursion >= MaxRecursion) {
        break;
      }
      List<String> sourceType = Arrays.asList(userIDS, tweetIDS).get(random.nextInt(0, 2));
      List<String> targetType = Arrays.asList(userIDS, tweetIDS).get(random.nextInt(0, 2));
      if (sourceType.equals(userIDS) & targetType.equals(userIDS)) {
        action = "follows";
      } else if (sourceType.equals(tweetIDS) & targetType.equals(userIDS)) {
        action = "cite";
      } else if (sourceType.equals(userIDS) & targetType.equals(tweetIDS)) {
        action = "retweet";
      } else {
        ++recursion;
        --j;
        continue;
      }
      source = sourceType.get(random.nextInt(0, sourceType.size()));
      target = targetType.get(random.nextInt(0, targetType.size()));
      edgeID = source + target;
      if (source.equals(target)) {
        ++recursion;
        --j;
        continue;
      }
      if (edgeIDs.contains(edgeID)) {
        ++recursion;
        --j;
        continue;
      }
      recursion = 0; //if doesn't enter in loop, reset
      edgeIDs.add(edgeID);
      allEdges.add(Arrays.asList("edge_id(" + edgeID + ")", "edge(" + source + "," + target + "," + edgeID + ")", "action(" + edgeID + "," + action + ")"));
    }

    List<String> graphDescription = new ArrayList<>();
    for (int j = 0; j < nNodeAttributes; ++j) {
      for (List<String> oneNode : allNodes) {
        graphDescription.add(oneNode.get(j));
      }
    }

    for (int j = 0; j < nArcAttributes; ++j) {
      for (List<String> oneEdge : allEdges) {
        graphDescription.add(oneEdge.get(j));
      }
    }

    for (String fact : graphDescription) {
      Query.hasSolution("assert(" + fact + ").");
    }

    return PrologGraphUtils.buildGraph(domainDefinition);
  }

  private static List<LinkedHashMap<String, Object>> analyseTwitterGeneration(int dimension, int nGraphs, int nOperations, List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();
    PrologGraph graph;
    for (int i = 0; i < nGraphs; ++i) {
      BasicGraphsAnalysis.resetProlog(factsNames);
      graph = generateTwitterGraph(dimension, domainDefinition);

      for (int j = 0; j < nOperations; ++j) {
        LinkedHashMap<String, Object> observation = new LinkedHashMap<>();
        Random rand = new Random();
        final int randomIndex = rand.nextInt(0, operators.size());
        String randomOperator = operators.get(randomIndex);
        final int previousDimension = graph.nodes().size() + graph.arcs().size();
        Instant startingInstant = Instant.now();
        graph = PrologGraphUtils.applyOperator(randomOperator, graph, domainDefinition, structuralRules);
        Instant endInstant = Instant.now();
        observation.put("graph", i);
        observation.put("operator", operatorsLabels.get(randomIndex));
        observation.put("dimension", previousDimension);
        observation.put("executionTime", Duration.between(startingInstant, endInstant).toNanos() / 1000000000d);
        DataFrame.add(observation);
      }
    }
    return DataFrame;
  }

  private static void exportTwitterAnalysis(List<String> operators, List<String> operatorsLabels, List<String> factsNames, List<String> domainDefinition, List<String> structuralRules) {
    int nGraphs = 25;
    int nOperations = 40;

    int dimension = 10;
    List<LinkedHashMap<String, Object>> DataFrame10 = analyseTwitterGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 25;
    List<LinkedHashMap<String, Object>> DataFrame25 = analyseTwitterGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 40;
    List<LinkedHashMap<String, Object>> DataFrame40 = analyseTwitterGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    dimension = 55;
    List<LinkedHashMap<String, Object>> DataFrame55 = analyseTwitterGeneration(dimension, nGraphs, nOperations, operators, operatorsLabels, factsNames, domainDefinition, structuralRules);

    String[] files = {"Dataframe10.csv", "Dataframe25.csv", "Dataframe40.csv", "Dataframe55.csv"};
    List<List<LinkedHashMap<String, Object>>> dfCollection = new ArrayList<>();
    dfCollection.add(DataFrame10);
    dfCollection.add(DataFrame25);
    dfCollection.add(DataFrame40);
    dfCollection.add(DataFrame55);

    for (int i = 0; i < dfCollection.size(); ++i) {
      String fileName = "TwitterComplete" + files[i];
      List<LinkedHashMap<String, Object>> df = dfCollection.get(i);

      try {
        // create a writer
        Writer writer = Files.newBufferedWriter(Paths.get("C:\\Users\\Simone\\Desktop\\GitHub_Tesi\\jgea_data\\25x40\\" + fileName));

        // write CSV file
        CSVPrinter printer = CSVFormat.DEFAULT.withHeader("graph", "operator", "dimension", "executionTime").print(writer);

        for (LinkedHashMap<String, Object> map : df) {
          printer.printRecord(map.get("graph"), map.get("operator"), map.get("dimension"), map.get("executionTime"));
        }

        // flush the stream
        printer.flush();

        // close the writer
        writer.close();

      } catch (IOException ex) {
        ex.printStackTrace();
      }
    }


  }

  public static void main(String[] args) {
    // Twitter subset definition:
    final List<String> domainDefinition = Arrays.asList(":- dynamic node_id/1.",
            ":- dynamic type/2.",
            ":- dynamic edge_id/1.",
            ":- dynamic edge/3.",
            ":- dynamic action/2.");

    final List<String> structuralRules = Arrays.asList(
            "action_value(retweet).",
            "action_value(post).",
            "action_value(cite).",
            "action_value(follows).",
            "type_value(user).",
            "type_value(tweet).",
            "cite_check(X) :- action(X,cite),edge(S,T,X), type(S,tweet), type(T,user).",
            "post_check(X) :- action(X,post),edge(S,T,X), type(S,user), type(T,tweet).",
            "retweet_check(X) :- action(X,retweet),edge(S,T,X), type(S,user), type(T,tweet).",
            "follows_check(X) :- action(X,follows),edge(S,T,X), type(S,user), type(T,user), S \\== T.",
            "retract_list([X | Xs], P) :- Z =.. [P, X], retract(Z), retract_list(Xs, P).",
            "retract_list([], _) :- true.",
            "retract_list([X|Xs],P,S) :- Z=.. [P,X,S], retract(Z), retract_list(Xs,P,S).",
            "retract_list([],_,S) :- true.",
            "tweet_indeg(T) :- findall(S, (edge(S,T,X),action(X,post)), Sources), length(Sources,1).",
            "is_valid :- foreach(findall(E,(edge_id(E),action(E,cite)),E1), maplist(cite_check,E1))," +
                    "   foreach(findall(E,(edge_id(E),action(E,post)),E2), maplist(post_check,E2))," +
                    "   foreach(findall(E,(edge_id(E),action(E,retweet)),E3), maplist(retweet_check,E3))," +
                    "   foreach(findall(E,(edge_id(E),action(E,follows)),E4), maplist(follows_check,E4)).",
            "is_valid2 :- foreach(findall(T,(node_id(T),type(T,tweet)),Tweets), maplist(tweet_indeg,Tweets))."
    );

    final List<String> factsNames = Arrays.asList("node_id/1", "type/2", "edge_id/1", "edge/3", "action/2");

    // Operators:
    List<String> operators = new ArrayList<>();
    List<String> operatorsLabels = new ArrayList<>();

    String addUser = "gensym(nod,X)," +
            "assert(node_id(X))," +
            "assert(type(X,user)).";
    operators.add(addUser);
    operatorsLabels.add("addUser");

    String addLegalTweet = "findall(N,type(N,user),UsersID)," +
            "random_member(Y,UsersID)," +
            "gensym(nod,X)," +
            "assert(node_id(X))," +
            "assert(type(X,tweet))," +
            "gensym(edg,E)," +
            "assert(edge_id(E))," +
            "assert(edge(Y,X,E))," +
            "assert(action(E,post)).";
    operators.add(addLegalTweet);
    operatorsLabels.add("addLegalTweet");

    String addRandomNode = "gensym(nod,X)," +
            "assert(node_id(X))," +
            "(maybe -> assert(type(X,user));" +
            "  assert(type(X,tweet))," +
            "  findall(N,type(N,user),UsersID)," +
            "  random_member(Y,UsersID)," +
            "  gensym(edg,E)," +
            "  assert(edge_id(E))," +
            "  assert(edge(Y,X,E))," +
            "  assert(action(E,post))" +
            ").";
    operators.add(addRandomNode);
    operatorsLabels.add("addRandomNode");

    String addLegalEdge = "findall(Z,node_id(Z),N)," +
            "random_member(X,N)," +
            "random_member(Y,N)," +
            "(edge(X,Y,_) ->  true; " +
            "    gensym(edge,E)," +
            "    assert(edge_id(E))," +
            "    assert(edge(X,Y,E))," +
            "    (type(X,user),type(Y,user),X \\==Y -> " +
            "            assert(action(E,follows)) ;" +
            "    type(X,tweet),type(Y,user) ->" +
            "        assert(action(E,cite)) ;" +
            "    type(X,user),type(Y,tweet) -> " +
            "       assert(action(E,retweet));" +
            "    retract(edge_id(E))," +
            "    retract(edge(X,Y,E))" +
            "    )" +
            ").";
    operators.add(addLegalEdge);
    operatorsLabels.add("addLegalEdge");

    String removeNode = "findall(Id,node_id(Id),Nodes)," +
            "random_member(N,Nodes)," +
            "findall(E,edge(_,N,E),ID_in)," +
            "findall(F,edge(N,_,F),ID_out)," +
            "retract_list(ID_out,action,_)," +
            "retract_list(ID_in,action,_)," +
            "retract_list(ID_in,edge_id)," +
            "retract_list(ID_out,edge_id)," +
            "retractall(edge(_,N,_))," +
            "retractall(edge(N,_,_))," +
            "retract(type(N,_))," +
            "retract(node_id(N)).";
    operators.add(removeNode);
    operatorsLabels.add("removeNode");

    String removeEdge = "findall(EID,edge_id(EID),Ids)," +
            "random_member(Removable,Ids)," +
            "retract(edge_id(Removable))," +
            "retract(action(Removable,_))," +
            "retract(edge(_,_,Removable)).";
    operators.add(removeEdge);
    operatorsLabels.add("removeEdge");

    String intermediatePublisher = "findall((S,T,I),(edge(S,T,I),action(I,post)),Edges)," +
            "random_member((N1,N2,ID),Edges)," +
            "retract(edge_id(ID))," +
            "retract(edge(N1,N2,ID))," +
            "retract(action(ID,_))," +
            "gensym(edg,E1)," +
            "gensym(edg,E2)," +
            "assert(edge_id(E1))," +
            "assert(edge_id(E2))," +
            "assert(action(E1,follows))," +
            "assert(action(E2,post))," +
            "gensym(nod,N)," +
            "assert(node_id(N))," +
            "assert(type(N,user))," +
            "assert(edge(N1,N,E1))," +
            "assert(edge(N,N2,E2)).";
    operators.add(intermediatePublisher);
    operatorsLabels.add("intermediatePublisher");

//    //Export CSV
//    exportTwitterAnalysis(operators, operatorsLabels, factsNames, domainDefinition, structuralRules);
    

  }

}
