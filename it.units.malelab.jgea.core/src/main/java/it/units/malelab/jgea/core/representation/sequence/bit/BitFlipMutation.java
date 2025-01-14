/*
 * Copyright 2020 Eric Medvet <eric.medvet@gmail.com> (as eric)
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

package it.units.malelab.jgea.core.representation.sequence.bit;

import it.units.malelab.jgea.core.operator.Mutation;

import java.util.random.RandomGenerator;

/**
 * @author eric
 */
public class BitFlipMutation implements Mutation<BitString> {

  private final double p;

  public BitFlipMutation(double p) {
    this.p = p;
  }

  @Override
  public BitString mutate(BitString parent, RandomGenerator random) {
    BitString newG = BitString.copyOf(parent);
    for (int i = 0; i < newG.size(); i++) {
      if (random.nextDouble() <= p) {
        newG.flip(i);
      }
    }
    return newG;
  }

}
