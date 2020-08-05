/*
 * Copyright (C) 2020 Eric Medvet <eric.medvet@gmail.com> (as eric)
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *  See the GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.units.malelab.jgea.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * @author eric
 */
public interface IndependentFactory<T> extends Factory<T> {

  @Override
  default List<T> build(int n, Random random) {
    List<T> ts = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      ts.add(build(random));
    }
    return ts;
  }

  T build(Random random);

  static <K> IndependentFactory<K> picker(K... ks) {
    return random -> ks[random.nextInt(ks.length)];
  }

  static <K> IndependentFactory<K> oneOf(IndependentFactory<K>... factories) {
    return random -> factories[random.nextInt(factories.length)].build(random);
  }

}
