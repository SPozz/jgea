package it.units.malelab.jgea.core.representation.graph.prolog;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Random;

public class AnalysisUtils {
  private static List<LinkedHashMap<String, Object>> analysis(int minDimension, int maxDimension, int nGraphs, int nOperations, PrologGraph originGraph, List<List<String>> operators, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> DataFrame = new ArrayList<>();
    Random random = new Random();

    List<String> factoryOperators = new ArrayList<>();
    for (List<String> labeledOperator : operators) {
      factoryOperators.add(labeledOperator.get(1));
    }

    List<PrologGraph> graphs = new PrologGraphFactory(minDimension, maxDimension, originGraph, factoryOperators, domainDefinition, structuralRules).build(nGraphs, random);

    for (int i = 0; i < graphs.size(); ++i) {
      PrologGraph graph = graphs.get(i);
      for (int j = 0; j < nOperations; ++j) {
        LinkedHashMap<String, Object> observation = new LinkedHashMap<>();
        int randomIndex = random.nextInt(0, operators.size());

        String randomOperator = operators.get(randomIndex).get(1);
        String label = operators.get(randomIndex).get(0);

        int dimension = graph.nodes().size() + graph.arcs().size();
        Instant startingInstant = Instant.now();
        graph = PrologGraphUtils.applyOperator(randomOperator, graph, domainDefinition, structuralRules);
        Instant endInstant = Instant.now();

        observation.put("graph", i);
        observation.put("operator", label);
        observation.put("dimension", dimension);
        observation.put("executionTime", Duration.between(startingInstant, endInstant).toNanos() / 1000000000d);

        DataFrame.add(observation);
      }
    }
    return DataFrame;
  }

  protected static void exportAnalysis(String fileName, int minDimension, int maxDimension, int nGraphs, int nOperations, PrologGraph originGraph, List<List<String>> operators, List<String> domainDefinition, List<String> structuralRules) {
    List<LinkedHashMap<String, Object>> df = analysis(minDimension, maxDimension, nGraphs, nOperations, originGraph, operators, domainDefinition, structuralRules);

    try {
      // create a writer
      Writer writer = Files.newBufferedWriter(Paths.get("./prolog/results/timeExecution/" + fileName));

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