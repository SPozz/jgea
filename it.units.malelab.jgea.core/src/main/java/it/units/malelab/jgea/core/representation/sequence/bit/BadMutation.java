package it.units.malelab.jgea.core.representation.sequence.bit;

import it.units.malelab.jgea.core.operator.Mutation;

import java.util.random.RandomGenerator;

public class BadMutation implements Mutation<BitString> {

  @Override
  public BitString mutate(BitString parent, RandomGenerator random) {
    BitString newG = BitString.copyOf(parent);
    for (int index = 0; index < parent.size(); index++) {
      if (parent.get(index).equals(true)) {
        newG.set(index, false);
        return newG;
      }
    }
    return newG;
  }

}