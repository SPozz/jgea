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

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package it.units.malelab.jgea.representation.tree;

import it.units.malelab.jgea.core.util.Sized;

import java.io.PrintStream;
import java.io.Serializable;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * @author eric
 */
public class Tree<C> implements Serializable, Sized, Iterable<Tree<C>> {

  private final C content;
  private final List<Tree<C>> children = new ArrayList<>();
  private Tree<C> parent;

  private Tree(C content, Tree<C> parent) {
    this.content = content;
    this.parent = parent;
  }

  public static <K> Tree<K> copyOf(Tree<K> other) {
    Tree<K> t = new Tree<>(other.content, null);
    for (Tree<K> child : other.children) {
      t.addChild(Tree.copyOf(child));
    }
    return t;
  }

  public static <K, H> Tree<H> map(Tree<K> other, Function<K, H> mapper) {
    Tree<H> t = Tree.of(mapper.apply(other.content));
    for (Tree<K> child : other.children) {
      t.addChild(Tree.map(child, mapper));
    }
    return t;
  }

  public static <K> Tree<K> of(K content) {
    return new Tree<>(content, null);
  }

  public static <K> Tree<K> of(K content, List<Tree<K>> children) {
    Tree<K> t = new Tree<>(content, null);
    for (Tree<K> child : children) {
      t.addChild(child);
    }
    return t;
  }

  private static <K> void prettyPrint(Tree<K> t, int d, PrintStream ps) {
    ps.printf(
        "%s (h=%2d d=%2d #c=%2d) %s",
        Collections.nCopies(d, "  ").stream().collect(Collectors.joining()),
        t.height(),
        t.depth(),
        t.nChildren(),
        t.content()
    );
    ps.println();
    t.forEach(c -> prettyPrint(c, d + 1, ps));
  }

  public void addChild(Tree<C> child) {
    children.add(child);
    child.parent = this;
  }

  public Tree<C> child(int i) {
    return children.get(i);
  }

  public Stream<Tree<C>> childStream() {
    return StreamSupport.stream(spliterator(), false);
  }

  public void clearChildren() {
    children.clear();
  }

  public C content() {
    return content;
  }

  public int depth() {
    if (parent == null) {
      return 0;
    }
    return parent.depth() + 1;
  }

  @Override
  public int hashCode() {
    return Objects.hash(content, children);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null || getClass() != o.getClass())
      return false;
    Tree<?> tree = (Tree<?>) o;
    return Objects.equals(content, tree.content) && children.equals(tree.children);
  }

  @Override
  public String toString() {
    return content.toString() + (children.isEmpty() ? "" : ("[" + children.stream().map(Tree::toString).collect(
        Collectors.joining(",")) + "]"));
  }

  public int height() {
    return 1 + children.stream().mapToInt(Tree::height).max().orElse(0);
  }

  public boolean isLeaf() {
    return children.isEmpty();
  }

  @Override
  public Iterator<Tree<C>> iterator() {
    return children.iterator();
  }

  public List<Tree<C>> leaves() {
    if (children.isEmpty()) {
      return List.of(this);
    }
    List<Tree<C>> leaves = new ArrayList<>();
    children.forEach(c -> leaves.addAll(c.leaves()));
    return leaves;
  }

  public int nChildren() {
    return children.size();
  }

  public Tree<C> parent() {
    return parent;
  }

  public void prettyPrint(PrintStream ps) {
    prettyPrint(this, 0, ps);
  }

  public boolean removeChild(Tree<C> child) {
    return children.remove(child);
  }

  @Override
  public int size() {
    return 1 + children.stream().mapToInt(Tree::size).sum();
  }

  public List<Tree<C>> topSubtrees() {
    List<Tree<C>> subtrees = new ArrayList<>();
    subtrees.add(this);
    children.forEach(c -> subtrees.addAll(c.topSubtrees()));
    return subtrees;
  }

  public List<C> visitDepth() {
    List<C> contents = new ArrayList<>(1 + children.size());
    contents.add(content);
    children.forEach(c -> contents.addAll(c.visitDepth()));
    return contents;
  }

  public List<C> visitLeaves() {
    return leaves().stream().map(Tree::content).toList();
  }

  public int maxArity() {
    int maxArity = 0;
    for (Tree<?> t : this) {
      if (t.nChildren() > maxArity) {
        maxArity = t.nChildren();
      }
    }
    return maxArity;
  }

  public int getNodeIndex(int maxArity) {
    if (parent == null) {
      return 0;
    }
    return maxArity * parent.getNodeIndex(maxArity) + 1 + parent.children.indexOf(this);
  }

  public static <K, H> Tree<H> mapFromIndex(Tree<K> other, BiFunction<K, Integer, H> mapper, int maxArity) {
    Tree<H> t = Tree.of(mapper.apply(other.content, other.getNodeIndex(maxArity)));
    for (Tree<K> child : other.children) {
      t.addChild(Tree.mapFromIndex(child, mapper, maxArity));
    }
    return t;
  }

}
