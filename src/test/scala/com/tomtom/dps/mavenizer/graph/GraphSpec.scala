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

import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.prop.PropertyChecks
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that our "transitive simplifier" graph algorithm works as expected. In particular,
 * we check a bunch of increasingly complicated graphs for both "levelization" and "normalization".
 *
 * @author Eric Bowman
 */
@RunWith(classOf[JUnitRunner])
class GraphSpec extends FeatureSpec with GivenWhenThen with PropertyChecks with Graph[String] {
  type ? = this.type

  val DigitExtract = """(\d+).*""".r

  type Node = TestNode

  case class TestNode(key: String) extends AbstractNode {

    /**
     * Lets us describe edges in the graph, like TestNode("1") -> TestNode("2").
     */
    def ->(other: TestNode) {
      addDependency(this, other)
    }

    /**
     * Used to assert that this node depends on exactly the set of nodes supplied,
     * and that each of those nodes has this node as a dependent.
     */
    def ==>(nodes: Set[TestNode]) {
      assert(dependencies(this) === nodes)
      nodes foreach {
        dependants(_).contains(this)
      }
    }

    override def toString = key + "_" + level
  }

  /**
   * Turns "1" into TestNode("1") implicitly, so we can write "1" -> "2".
   * Note that it doesn't create a TestNode, it asks the Graph to give it
   * the named node; the Graph will create one if it doesn't already exist via mkNode.
   */
  implicit def strToTestNode(str: String) = node(str)

  /**
   * Implicitly turns "1" into Set(TestNode("1")) so we can write "1" ==> "2".
   */
  implicit def strToTestNodeSet(str: String) = Set(node(str))

  /**
   * Implicitly turns Set("1","2") into Set(TestNode("1"), TestNode("2")) so
   * we can write "1" ==> Set("2", "3")
   */
  implicit def strsToTestNodeSet(str: Set[String]) = str.map(node(_))

  /**
   * Factory method Graph needs in order to create new TestNode instances.
   */
  def mkNode(key: String) = TestNode(key)

  /**
   * To test graph leveling, all our nodes are given a name that starts with a number;
   * that number is the level we expect the node to end up with after the leveling operation,
   * making it really easy to verify whether the test passed or not.
   */
  feature("Graph level operation") {

    scenario(
      """|should level this test graph so that levels match the node names,
         | +---+     +---+
         | | 1 | --> | 0 |
         | +---+     +---+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1" -> "0"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }

    scenario(
      """|should level this test graph so that levels match the node names,
         |  +---+     +---+     +---+
         |  | 2 | --> | 1 | --> | 0 |
         |  +---+     +---+     +---+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1" -> "0"
        "2" -> "1"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }

    scenario(
      """|should level this test graph so that levels match the node names,
         |  +----+     +---+     +----+
         |  | 1a | --> | 0 | <-- | 1b |
         |  +----+     +---+     +----+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1a" -> "0"
        "1b" -> "0"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }

    scenario(
      """|should level this test graph so that levels match the node names,
         |  +----+     +----+     +---+
         |  | 2  | --> | 1b | --> | 0 |
         |  +----+     +----+     +---+
         |    |                     ^
         |    |                     |
         |    v                     |
         |  +----+                  |
         |  | 1a | -----------------+
         |  +----+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "2" -> "1b"
        "2" -> "1a"
        "1b" -> "0"
        "1a" -> "0"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }

    scenario(
      """|should level this test graph so that levels match the node names,
         |  +----+     +---+     +----+     +---+
         |  | 3  | --> | 2 | --> | 1a | --> | 0 |
         |  +----+     +---+     +----+     +---+
         |    |                               ^
         |    |                               |
         |    v                               |
         |  +----+                            |
         |  | 1b | ---------------------------+
         |  +----+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "3" -> "2"
        "2" -> "1a"
        "1a" -> "0"
        "3" -> "1b"
        "1b" -> "0"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }

    scenario(
      """|should level this test graph so that levels match the node names,
         |  +----+     +---+     +----+
         |  | 2  | --> | 1 | --> | 0a |
         |  +----+     +---+     +----+
         |    |
         |    |
         |    v
         |  +----+
         |  | 0b |
         |  +----+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "2" -> "1"
        "1" -> "0a"
        "2" -> "0b"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }

    scenario(
      """|should level this test graph so that levels match the node names,
         |       +---------------+
         |       |               v
         |       |  +----+     +----+     +----+
         |       |  | 2c | --> | 1  | --> | 0  |
         |       |  +----+     +----+     +----+
         |       |               ^
         |       |               +-------------------------------------------+
         |       |                                                           |
         |       |  +----+     +----+     +----+     +----+     +----+     +----+
         |       |  | 7b | --> | 6b | --> | 5b | --> | 4b | --> | 3b | --> | 2b |
         |       |  +----+     +----+     +----+     +----+     +----+     +----+
         |       |    |                     |
         |       |    |                     |
         |       |    v                     v
         |       |  +----+                +----+
         |  +----+> | 0b |                | 0a |
         |  |    |  +----+                +----+
         |  |    |
         |  |    +------------------------------------------------+
         |  |                                                     |
         |  |       +----+     +----+     +----+     +----+     +----+
         |  |       | 6a | --> | 5a | --> | 4a | --> | 3a | --> | 2a |
         |  |       +----+     +----+     +----+     +----+     +----+
         |  |                    |
         |  +--------------------+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1" -> "0"
        "2a" -> "1"
        "3a" -> "2a"
        "4a" -> "3a"
        "5a" -> "4a"
        "6a" -> "5a"
        "2b" -> "1"
        "3b" -> "2b"
        "4b" -> "3b"
        "5b" -> "4b"
        "6b" -> "5b"
        "7b" -> "6b"
        "2c" -> "1"
        "5b" -> "0a"
        "5b" -> "0a"
        "5a" -> "0b"
        "7b" -> "0b"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }

    scenario(
      """|should level this test graph so that levels match the node names,
         |                             +-------------------------+
         |                             |                         |
         |                             |                         |
         |                  +----------+--------------------+    |
         |                  |          |                    v    |
         |                +----+     +----+     +---+     +---+  |
         |                | 3a | --> | 2a | --> |   | --> | 0 | <+
         |                +----+     +----+     |   |     +---+
         |                  |                   |   |
         |                  +-----------------> | 1 |
         |                                      |   |
         |     +----+     +----+                |   |
         |  +- | 3b | <-- | 4b | -------------> |   | <+
         |  |  +----+     +----+                +---+  |
         |  |    |          |                     ^    |
         |  |    |          |                     |    |
         |  |    |          v                     |    |
         |  |    |        +----+                  |    |
         |  +----+------> | 2b | -----------------+    |
         |       |        +----+                       |
         |       |                                     |
         |       +-------------------------------------+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1" -> "0"
        "2a" -> "0"
        "2a" -> "1"
        "3a" -> "0"
        "3a" -> "1"
        "3a" -> "2a"
        "2b" -> "1"
        "3b" -> "1"
        "3b" -> "2b"
        "4b" -> "1"
        "4b" -> "2b"
        "4b" -> "3b"
      }
      when("we level it")
      level()
      then("the level convention should be verified")
      verifyLevelCondition()
    }
  }

  /**
   * To test whether normalization works, is harder to automate in the same way.
   * So here we have a DSL using ==> to make it easy to express what the direct dependencies
   * should be after normalization.
   */
  feature("Graph normalizeDependencies") {

    scenario(
      """|should normalize this test graph,
         | +---+     +---+
         | | 1 | --> | 0 |
         | +---+     +---+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1" -> "0"
      }
      when("we normalize it")
      normalizeDependencies()
      then(
        """|then it should match
           | +---+     +---+
           | | 1 | --> | 0 |
           | +---+     +---+
           |""".stripMargin)
      "1" ==> "0"
    }

    scenario(
      """|should normalize this test graph,
         |  +---+     +---+     +---+
         |  | 2 | --> | 1 | --> | 0 |
         |  +---+     +---+     +---+
         | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1" -> "0"
        "2" -> "1"
      }
      when("we normalize it")
      normalizeDependencies()
      then(
        """|then it should match
           |  +---+     +---+     +---+
           |  | 2 | --> | 1 | --> | 0 |
           |  +---+     +---+     +---+
           |""".stripMargin)
      "1" ==> "0"
      "2" ==> "1"
    }

    scenario(
      """
      |should normalize this test graph,
      |    +---------------------+
      |    |                     v
      |  +----+     +----+     +---+
      |  | 2  | --> | 1b | --> | 0 |
      |  +----+     +----+     +---+
      |    |                     ^
      |    |                     |
      |    v                     |
      |  +----+                  |
      |  | 1a | -----------------+
      |  +----+
      | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "2" -> "0"
        "2" -> "1b"
        "2" -> "1a"
        "1a" -> "0"
        "1b" -> "0"
      }
      when("we normalize it")
      normalizeDependencies()
      then(
        """
        |then it should match
        |  +----+     +----+     +---+
        |  | 2  | --> | 1b | --> | 0 |
        |  +----+     +----+     +---+
        |    |                     ^
        |    |                     |
        |    v                     |
        |  +----+                  |
        |  | 1a | -----------------+
        |  +----+
        |""".stripMargin)
      "0" ==> Set()
      "2" ==> Set("1a", "1b")
      "1a" ==> "0"
      "1b" ==> "0"
    }

    scenario(
      """
      |should normalize this test graph,
      |                             +-------------------------+
      |                             |                         |
      |                             |                         |
      |                  +----------+--------------------+    |
      |                  |          |                    v    |
      |                +----+     +----+     +---+     +---+  |
      |                | 3a | --> | 2a | --> |   | --> | 0 | <+
      |                +----+     +----+     |   |     +---+
      |                  |                   |   |
      |                  +-----------------> | 1 |
      |                                      |   |
      |     +----+     +----+                |   |
      |  +- | 3b | <-- | 4b | -------------> |   | <+
      |  |  +----+     +----+                +---+  |
      |  |    |          |                     ^    |
      |  |    |          |                     |    |
      |  |    |          v                     |    |
      |  |    |        +----+                  |    |
      |  +----+------> | 2b | -----------------+    |
      |       |        +----+                       |
      |       |                                     |
      |       +-------------------------------------+
      | in the expected way.""".stripMargin) {
      given("our test graph")
      graph {
        "1" -> "0"
        "2a" -> "0"
        "2a" -> "1"
        "3a" -> "0"
        "3a" -> "1"
        "3a" -> "2a"
        "2b" -> "1"
        "3b" -> "1"
        "3b" -> "2b"
        "4b" -> "1"
        "4b" -> "2b"
        "4b" -> "3b"
      }
      when("we normalize it")
      normalizeDependencies()
      then(
        """
        |then it should match
        |  +----+     +----+     +----+     +---+
        |  | 3a | --> | 2a | --> | 1  | --> | 0 |
        |  +----+     +----+     +----+     +---+
        |                          ^
        |                          |
        |                          |
        |  +----+     +----+     +----+
        |  | 4b | --> | 3b | --> | 2b |
        |  +----+     +----+     +----+
        |""".stripMargin)
      "0" ==> Set()
      "1" ==> "0"
      "2a" ==> "1"
      "3a" ==> "2a"
      "2b" ==> "1"
      "3b" ==> "2b"
      "4b" ==> "3b"
    }
  }

  feature("Cycle detection") {
    scenario("should detect a cycles when they are present") {
      given("a graph with a cycle")
      graph {
        "1" -> "2"
        "2" -> "3"
        "3" -> "1"
      }
      when("we call assertNoCycle")
      intercept[RuntimeException] {
        assertNoCycles()
      }
      then("it should throw")
    }
  }


  /**
   * A leveled graph in this test, by convention, has a name that starts with a number,
   * indicating its expected level This method asserts that condition is true for all
   * nodes in the graph.
   */
  def verifyLevelCondition() {
    nodes.values.foreach {
      node => node.key match {
        case DigitExtract(nameNumPrefix) =>
          assert(nameNumPrefix.toInt === node.level)
      }
    }
  }

  /**
   * Helper method to clear the graph, then define it via the supplied code block.
   */
  def graph(x: => Unit) {
    clear()
    x
  }
}
