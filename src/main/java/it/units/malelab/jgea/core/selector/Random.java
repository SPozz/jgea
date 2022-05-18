package it.units.malelab.jgea.core.selector;

import it.units.malelab.jgea.core.order.PartiallyOrderedCollection;
import it.units.malelab.jgea.core.util.Misc;

import java.util.random.RandomGenerator;

/**
 * @author giorgia
 */
public class Random implements Selector<Object> {

  @Override
  public <K> K select(PartiallyOrderedCollection<K> ks, RandomGenerator random) {
    return Misc.pickRandomly(ks.all(), random);
  }

  @Override
  public String toString() {
    return "Random{" + '}';
  }

}
