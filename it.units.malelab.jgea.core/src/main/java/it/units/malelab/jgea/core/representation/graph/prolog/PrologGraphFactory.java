package it.units.malelab.jgea.core.representation.graph.prolog;

import it.units.malelab.jgea.core.Factory;
import it.units.malelab.jgea.core.util.Misc;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.random.RandomGenerator;

public class PrologGraphFactory implements Factory<PrologGraph> {
  final private int minDimension;
  final private int maxDimension;
  final private PrologGraph originGraph;
  final private List<String> operators;
  final private List<String> domainDefinition;
  final private List<String> structuralRules;

  public PrologGraphFactory(int minDimension, int maxDimension, PrologGraph originGraph, List<String> operators, List<String> domainDefinition, List<String> structuralRules) {
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
    final int maxAttempts = 150;
    final int nStep = maxDimension - minDimension + 1;
    final int graphPerDimension = n / nStep;

    int[] dimensions = new int[n];
    for (int j = 0; j < nStep; ++j)
      for (int i = 0; i < graphPerDimension; ++i)
        dimensions[graphPerDimension * j + i] = minDimension + j;
    for (int i = 1; i <= (n - nStep * graphPerDimension); ++i)
      dimensions[n - i] = new Random().nextInt(minDimension, maxDimension + 1);

    for (final int dimension : dimensions) {
      PrologGraph graph = originGraph;
      int attempt = 0;
      while (graph.size() < dimension && attempt < maxAttempts) {
        String operator = Misc.pickRandomly(operators, random);
        graph = PrologGraphUtils.applyOperator(operator, graph, domainDefinition, structuralRules);
        attempt++;
      }
      graphList.add(graph);
    }
    return graphList;
  }


  public static void exportFactoryAnalysis(String domainName, int minDimension, int maxDimension, PrologGraph originGraph, List<String> operators, List<String> domainDefinition, List<String> structuralRules) {
    final int nGraphs = 200;
    final Random random = new Random();

    List<PrologGraph> graphs = new PrologGraphFactory(minDimension, maxDimension, originGraph, operators, domainDefinition, structuralRules).build(nGraphs, random);

    String fileName = domainName + minDimension + "To" + maxDimension;

    try {
      // create a writer
      Writer writer = Files.newBufferedWriter(Paths.get("C:\\Users\\Simone\\Desktop\\GitHub_Tesi\\jgea_data\\sizeDistribution\\" + fileName + ".csv"));

      // write CSV file
      CSVPrinter printer = CSVFormat.DEFAULT.withHeader("graph", "dimension", "nNodes", "nArcs").print(writer);
      int index = 0;

      for (PrologGraph graph : graphs) {
        printer.printRecord(index, graph.size(), graph.nodes().size(), graph.arcs().size());
        index++;
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
