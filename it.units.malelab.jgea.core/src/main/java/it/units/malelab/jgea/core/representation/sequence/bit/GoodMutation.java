package it.units.malelab.jgea.core.representation.sequence.bit;

import it.units.malelab.jgea.core.operator.Mutation;

import java.util.random.RandomGenerator;

public class GoodMutation implements Mutation<BitString> {

  @Override
  public BitString mutate(BitString parent, RandomGenerator random) {
    BitString newG = BitString.copyOf(parent);
    int index = random.nextInt(newG.size());
    newG.set(index, true);
    return newG;
  }

}