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

package com.tomtom.dps.mavenizer.dependency

import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.prop.PropertyChecks
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import xml.NodeSeq
@RunWith(classOf[JUnitRunner])
class NexusSpec extends FeatureSpec with GivenWhenThen with PropertyChecks with NexusComponent {
  type ? = this.type
  val MAX: Int = 64

  val nexus = new NexusImpl(null)
  import nexus.makeArtifact

  feature("Missing data should result in no artifact being created by the Nexus.makeArtifact factory method") {
    scenario("A missing component should result in no JarArtifact") {
      given("an artifact xml")
      when("one or more of artifactId, groupId, or version is missing")
      then("the makeArtifact factory should return None")
      val cases = for (subset <- powerSet(List("artifactId", "groupId", "version"))) yield {
        for (elem <- subset) yield {
          elem match {
            case "artifactId" => <artifactId>
              {elem}
            </artifactId>
            case "groupId" => <groupId>
              {elem}
            </groupId>
            case "version" => <version>
              {elem}
            </version>
          }
        }
      }
      for (c <- cases) {
        val xml = <artifact>
          {NodeSeq.fromSeq(c)}
        </artifact>
        assert(None === makeArtifact(xml))
      }
      assert(None === makeArtifact(<artifact></artifact>))
    }
  }

  feature("Artifact xml can occur in any order and the same JarArtifact will be created") {
    scenario("For an artifact xml") {
      when("the artifactId, groupId, and version may arrive in any order")
      then("we can parse it correctly no matter the order")
      for (ordering <- List("artifactId", "groupId", "version").permutations) yield {
        val cases = for (elem <- ordering) yield {
          elem match {
            case "artifactId" => <artifactId>
              {elem}
            </artifactId>
            case "groupId" => <groupId>
              {elem}
            </groupId>
            case "version" => <version>
              {elem}
            </version>
          }
        }
        for (c <- cases) {
          assert(None != makeArtifact(<artifact>
            {NodeSeq.fromSeq(c)}
          </artifact>))
        }
      }
    }
  }

  /** Given Seq(1,2,3) generates Seq(Seq(1), Seq(2), Seq(3), Seq(1,2), Seq(2,3), Seq(1,3)). */
  def powerSet[X](xs: Seq[X]): Seq[Seq[X]] = {
    val xis = xs.zipWithIndex
    for (j <- 1 until (1 << (xs.length - 1))) yield {
      for ((x, i) <- xis if ((j & (1 << i)) != 0)) yield x
    }
  }
}