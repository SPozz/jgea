/*
 * Copyright 2022 eric
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package it.units.malelab.jgea.experimenter;

import it.units.malelab.jgea.experimenter.builders.*;
import it.units.malelab.jnb.core.NamedBuilder;

import java.util.List;

/**
 * @author "Eric Medvet" on 2022/11/24 for jgea
 */
public class PreparedNamedBuilder {

  private final static NamedBuilder<Object> NB = NamedBuilder.empty()
      .and(List.of("ea"), NamedBuilder.empty()
          .and(List.of("randomGenerator", "rg"), NamedBuilder.fromUtilityClass(RandomGenerators.class))
          .and(List.of("problem", "p"), NamedBuilder.fromUtilityClass(Problems.class))
          .and(List.of("solver", "s"), NamedBuilder.fromUtilityClass(Solvers.class))
          .and(List.of("listener", "l"), NamedBuilder.fromUtilityClass(Listeners.class))
          .and(List.of("function", "f"), NamedBuilder.fromUtilityClass(Functions.class))
          .and(List.of("namedFunction", "nf"), NamedBuilder.fromUtilityClass(NamedFunctions.class))
          .and(List.of("plot"), NamedBuilder.fromUtilityClass(Plots.class))
          .and(NamedBuilder.fromClass(Experiment.class))
          .and(NamedBuilder.fromClass(Run.class))
      );

  private PreparedNamedBuilder() {
  }

  public static NamedBuilder<Object> get() {
    return NB;
  }
}
