/*
 * Copyright 2011 TomTom International BV
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

package com.tomtom.dps.mavenizer.graph

/**
 * This uses "family polymorphism" to do some powerful type stuff not possible in Java.
 * See http://www.familie-kneissl.org/Members/martin/blog/family-polymorphism-in-scala.
 * 
 * This trait implements a graph algorithm to simplify a "brute force" dependency graph,
 * where every dependency is completely explicit, to a "transitive" dependency graph, where
 * a module C that depends on A and B, does not need to state explicitly a dependency on A,
 * if B also depends on A.
 * 
 * Note that this implementation is in an imperative style, making use of some advanced
 * features of Scala's type mechanism (family polymorphism), and its collection framework,
 * but doesn't try to be functional.
 *
 * @author Eric Bowman
 */
trait Graph[K /* K is the key type */] {

  /** Nodes have a level which we can set later; hence var. */
  trait AbstractNode {
    var level: Int = -1
  }

  /** Implementations of the trait must supply the Node type, which must extend AbstractNode. */
  type Node <: AbstractNode

  /** A map from keys, to nodes. Each node must have a unique key. */
  val nodes = collection.mutable.Map[K, Node]()
  
  /** A map from each node, to the nodes it depends on. */
  val dependencies = new collection.mutable.HashMap[Node, Set[Node]] {
    override def default(n: Node) = Set[Node]()
  }
  
  /** A map from each node, to the nodes that depend on it. */
  val dependants = new collection.mutable.HashMap[Node, Set[Node]] {
    override def default(n: Node) = Set[Node]()
  }

  /** Has this graph been level yet? Need to level before normalizing. */
  var leveled = false

  /** Returns the node for a given key, creating one if it hasn't been seen yet. */
  def node(key: K): Node = nodes.getOrElseUpdate(key, mkNode(key))

  /** Abstract factory method; trait implementors must supply this method. */
  def mkNode(key: K): Node

  /** Clears the graph so it may be used again. */
  def clear() {
    nodes.clear()
    dependencies.clear()
    dependants.clear()
    leveled = false
  }

  /** Used to express that one node depends on another node (for creating the graph). */
  def addDependency(aNode: Node, dependsOn: Node) {
    dependencies(aNode) += dependsOn
    dependants(dependsOn) += aNode
  }

  /** Computes the set of Node instances, which don't depend on any other nodes. */
  def roots: Set[Node] = nodes.values.filter(dependencies(_).size == 0).toSet

  /** Throws an exception if the graph has any cycles. */
  def assertNoCycles() {
    nodes.values.foreach {
      node: Node =>
        def recurse(n: Node, path: List[Node]) {
          if (n == node) {
            sys.error("Already seen node " + n + " while following leads from " + path)
          } else {
            this.dependencies(n).foreach(recurse(_, n :: path))
          }
        }
        this.dependencies(node).foreach(recurse(_, Nil))
    }
  }

  /** Generates a string of GraphViz DOT commands to display the graph, using toString on the key type K. */
  def toDOT: String = {
    val commands = for (from <- dependencies.keys; dep <- dependencies(from)) yield
      "[ " + from + " ] -> [ " + dep + " ]"
    commands.mkString("\n")
  }


  /**
   * Levels the DAG, by which we mean, each node is assigned a level, maximum path length from the set of all paths
   * from this node to all roots.
   */
  def level() {
    assertNoCycles()
    def recurse(node: Node, level: Int) {
      node.level = math.max(node.level, level)
      for (child <- dependants(node)) {
        recurse(child, level + 1)
      }
    }
    roots.foreach(recurse(_, 0))
    leveled = true
  }

  /**
   * The process of normalizing dependencies is meant to describe simplifying the dependency graph so that transitive
   * dependencies are implied, instead of explicit.  In particular, the dependency graph we build by inspecting the
   * build.xml scripts results in all dependencies between OFBiz submodules being extremely explicit.  Imagine modules
   * A, B, C, D, and E, and a possible dependency relationship between them:
   *
   *   +-----------------------------+
   *   |                             v
   * +---+     +---+     +---+     +---+     +---+
   * | E | --> | D | --> | C | --> | B | --> | A |
   * +---+     +---+     +---+     +---+     +---+
   *   |                                       ^
   *   +---------------------------------------+
   *
   * (Where E depends on D, B and A; D depends on C, C depends on B, and B depends on A).
   *
   * The dependency E->D implies E->B and E->A, by virtue of transitivity. In other words, B and A are "transitive
   * dependencies" of E.  We would like to simplify this by removing the direct dependency links E->A and E-B,
   * which modifies the graph to look like:
   *
   *   +---+     +---+     +---+     +---+     +---+
   *   | E | --> | D | --> | C | --> | B | --> | A |
   *   +---+     +---+     +---+     +---+     +---+
   *
   * How do we do that?  Well, here's one way we can understand. There may be more graph-theoretic approaches, but
   * this is sort of hand-wavy explainable.
   *
   * 1. Define a root node as a node with dependants by no dependencies. There may be more than one; in the
   * below A and B are both root nodes
   *
   *             +---+
   *             | B |
   *             +---+
   *               ^
   *               |
   *               |
   *   +---+     +---+     +---+
   *   | D | --> | C | --> | A |
   *   +---+     +---+     +---+
   *               ^
   *               |
   *               |
   *             +---+
   *             | E |
   *             +---+
   *
   * 2. Define the "level" of a node, as the longest distance from that node to any root node.  In the example
   * below, for instance, the level for E is 4 because the longest path to any node is 4, even though relative
   * to F, its level is 1.
   *
   *    +------+     +------+     +------+     +------+     +------+
   *    | E(4) | --> | D(3) | --> | C(2) | --> | B(1) | --> | A(0) |
   *    +------+     +------+     +------+     +------+     +------+
   *      |
   *      |
   *      v
   *    +------+
   *    | F(0) |
   *    +------+
   *
   * 3. For the problem at hand, we can assume that the system is in a state of NO transitive dependencies.
   * So, we can walk up to a node, and know everything it depends on, without having to travel through the graph.
   * This makes it pretty easy to get rid of the transitive dependencies.  The algorithm is something like this:
   *
   * for each node N that is not a root node,
   *   for each dependency Dy1 of N
   *      assert(N.level > Dy.level)
   *      for each dependency Dy2 of N, where Dy2.level > Dy1.level
   *        if (Dy2.deps - Dy1.deps) contains Dy1     <-- this is the magic bit
   *          sever Dy1
   *
   * The outcome of this algorithm, we claim with virtually no justification other than hand-waving, is a nice
   * dependency graph such that there is only one path between from any given node N, any dependency M, and every
   * root node R which is a dependency of M.
   */
  def normalizeDependencies() {
    assertNoCycles()
    if (!leveled) {
      level()
    }
    val rootNodes = roots
    val toSever = collection.mutable.Set[(Node, Node)]()
    for {n <- nodes.values if !rootNodes.contains(n)
         dy1 <- dependencies(n)
         dy2 <- dependencies(n) if dy2.level > dy1.level
    } {
      if (dependencies(dy2).diff(dependencies(dy1)).contains(dy1)) {
        toSever += ((n, dy1))
      }
    }
    for ((a, b) <- toSever) {
      dependencies(a) -= b
      dependants(b) -= a
    }
  }
}