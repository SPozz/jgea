package it.units.malelab.jgea.sample.lab.prolog;

import com.google.common.base.Stopwatch;
import it.units.malelab.jgea.core.QualityBasedProblem;
import it.units.malelab.jgea.core.listener.CSVPrinter;
import it.units.malelab.jgea.core.listener.ListenerFactory;
import it.units.malelab.jgea.core.listener.NamedFunction;
import it.units.malelab.jgea.core.listener.TabularPrinter;
import it.units.malelab.jgea.core.representation.sequence.UniformCrossover;
import it.units.malelab.jgea.core.representation.sequence.bit.*;
import it.units.malelab.jgea.core.selector.Last;
import it.units.malelab.jgea.core.selector.Tournament;
import it.units.malelab.jgea.core.solver.*;
import it.units.malelab.jgea.core.solver.state.POSetPopulationState;
import it.units.malelab.jgea.problem.synthetic.OneMax;
import it.units.malelab.jgea.sample.Worker;

import java.io.File;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import static it.units.malelab.jgea.core.listener.NamedFunctions.*;
import static it.units.malelab.jgea.core.listener.NamedFunctions.attribute;
import static it.units.malelab.jgea.sample.Args.i;
import static it.units.malelab.jgea.sample.Args.ri;

public class OneMaxExample extends Worker {

  public final static List<NamedFunction<? super POSetPopulationState<?, ?, ? extends Double>, ?>> functions =
          List.of(
                  iterations(),
                  births(),
                  elapsedSeconds(),
                  operatorsProbabilitiesPlot(4),
                  operatorProbability(0),
                  operatorProbability(1),
                  operatorProbability(2),
                  operatorProbability(3),
                  size().of(all()),
                  size().of(firsts()),
                  size().of(lasts()),
                  uniqueness().of(each(genotype())).of(all()),
                  uniqueness().of(each(solution())).of(all()),
                  uniqueness().of(each(fitness())).of(all()),
                  size().of(genotype()).of(best()),
                  size().of(solution()).of(best()),
                  fitnessMappingIteration().of(best()),
                  fitness().reformat("%5.3f").of(best()),
                  hist(8).of(each(fitness())).of(all()),
                  max(Double::compare).reformat("%5.3f").of(each(fitness())).of(all())
          );

  public OneMaxExample(String[] args) {
    super(args);
  }

  public static void main(String[] args) {
    new OneMaxExample(args);
  }

  @Override
  public void run() {
    int nPop = i(a("nPop", "100"));
    int nTournament = 5;
    int maxDiversityAttempts = 100;
    int maxIterations = i(a("nIterations", "50"));
    int[] seeds = ri(a("seed", "0:100"));
    int size = i(a("size", "200"));

    Random r = new Random(1);
    QualityBasedProblem<BitString, Double> p = new OneMax();

    List<NamedFunction<? super Map<String, Object>, ?>> kFunctions = List.of(
            attribute("seed").reformat("%2d"),
            attribute("size").reformat("%2d"),
            attribute("evolver").reformat("%20.20s")
    );
    ListenerFactory<POSetPopulationState<?, ?, ? extends Double>, Map<String, Object>> listenerFactory =
            new TabularPrinter<>(
                    functions,
                    kFunctions
            );

    listenerFactory = ListenerFactory.all(List.of(
            listenerFactory,
            new CSVPrinter<>(functions, kFunctions, new File("./prolog/results/oneMax-test2-" + size + ".csv"))
    ));


    List<IterativeSolver<? extends POSetPopulationState<?, BitString, Double>, QualityBasedProblem<BitString, Double>
            , BitString>> solvers = new ArrayList<>();
    Function<Long, Double> constantSchedulNull = x -> 0.0d;
    Function<Long, Double> constantScheduleHigh = x -> 0.025d;
    Function<Long, Double> constantScheduleLow = x -> 0.001d;
    Function<Long, Double> stepScheduleInit = x -> x < maxIterations / 2 ? 0.01 : 0;
    Function<Long, Double> stepScheduleEnd = x -> x > maxIterations / 2 ? 0.01 : 0;
//    List<Function<Long, Double>> schedules = Arrays.asList(constantScheduleHigh, constantScheduleLow, stepScheduleInit, stepScheduleEnd, constantSchedulNull);


    Function<Long, Double> constantScheduleVeryHigh = x -> 0.075d;
    Function<Long, Double> stepScheduleInitHigh = x -> x < maxIterations / 2 ? 0.075 : 0;
    Function<Long, Double> stepScheduleEndHigh = x -> x > maxIterations / 2 ? 0.075 : 0;
    List<Function<Long, Double>> schedules = Arrays.asList(constantSchedulNull, constantScheduleVeryHigh, stepScheduleEndHigh, stepScheduleInitHigh);

//    solvers.add(new StandardWithEnforcedDiversityEvolver<POSetPopulationState<BitString, BitString, Double>,
//            QualityBasedProblem<BitString, Double>, BitString, BitString, Double>(
//            Function.identity(),
//            new BitStringFactory(size),
//            nPop,
//            StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(maxIterations)),
//            Map.of(new UniformCrossover<>(new BitStringFactory(size)), 1d, new BitFlipMutation(0.01d), 1d),
//            new Tournament(nTournament),
//            new Last(),
//            nPop,
//            true,
//            false,
//            (problem, random) -> new POSetPopulationState<>(),
//            maxDiversityAttempts
//    ));

    for (Function<Long, Double> schedule : schedules) {
      solvers.add(
              new AdaptiveEvolver<>(
                      Function.identity(),
                      new BitStringFactory(size),
                      nPop,
                      StopConditions.targetFitness(0d).or(StopConditions.nOfIterations(maxIterations)),
                      Map.ofEntries(
                              Map.entry(new UniformCrossover<>(new BitStringFactory(size)), 1d),
                              Map.entry(new BitFlipMutation(0.01d), 1d),
                              Map.entry(new BadMutation(), 1d),
                              Map.entry(new GoodMutation(), 1d)
                      ),
                      new Tournament(nTournament),
                      new Last(),
                      nPop,
                      true,
                      false,
                      maxDiversityAttempts,
                      schedule
              ));
    }


    for (int seed : seeds) {
      for (IterativeSolver<? extends POSetPopulationState<?, BitString, Double>, QualityBasedProblem<BitString, Double>
              , BitString> evolver : solvers) {
        try {
          Stopwatch stopwatch = Stopwatch.createStarted();
          Map<String, Object> keys = Map.ofEntries(
                  Map.entry("seed", seed),
                  Map.entry("evolver", evolver.toString().substring("it.units.malelab.jgea.sample.lab.prolog".length())),
                  Map.entry("size", size)
          );

          L.info(String.format("Starting %s", keys));

          Collection<BitString> solutions = evolver.solve(
                  p,
                  r,
                  executorService,
                  listenerFactory.build(keys).deferred(executorService)
          );
          L.info(String.format(
                  "Done %s: %d solutions in %4.1fs",
                  keys,
                  solutions.size(),
                  (double) stopwatch.elapsed(TimeUnit.MILLISECONDS) / 1000d
          ));
        } catch (SolverException e) {
          e.printStackTrace();
        }
      }
    }


    listenerFactory.shutdown();


  }
}

