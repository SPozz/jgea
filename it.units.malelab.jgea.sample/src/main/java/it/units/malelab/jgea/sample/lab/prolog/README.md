# A General Purpose Representation and Adaptive EA for Evolving Graphs

Here we have the full code used to obtain all the results cited in our paper "A General Purpose Representation and Adaptive EA for Evolving Graphs" [our paper](https://medvet.inginf.units.it/publications/2023-c-mpm-general/)
This is an augmented version of Java General Evolutionary Algorithm (jgea) which is a modular Java framework for experimenting with Evolutionary Computation. [jgea](https://github.com/ericmedvet/jgea)


In particular:
- _ExtractionComparison.java_ is used for the experiments on text-extraction with deterministic-finite-automata
- _RegressionComparison.java_ is used for the experiments on symbolic regression with expression trees
- _ClassificationComparison.java_ is used for the experiments on classification (which are not presented in the paper)

The representation of graphs in Prolog is implemented in [prolog graph folder](https://github.com/SPozz/jgea/tree/features-graphs/it.units.malelab.jgea.core/src/main/java/it/units/malelab/jgea/core/representation/graph/prolog)

### Requirements
JPL .jar file as in  [our choice of JPL .jar](https://github.com/SPozz/jgea/blob/features-graphs/it.units.malelab.jgea.core/packages-jpl-8.4.1.jar)