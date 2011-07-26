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

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnitRunner])
class ArtifactFilterSpec extends Spec with ShouldMatchers {
  import ArtifactFilter.filterArtifacts
  describe("ArtifactFilter") {
    describe("should deal ok with an empty list") {
      filterArtifacts(Nil) should(be(Nil))
    }
    describe("should sort correctly") {
      filterArtifacts(List(JarArtifact("a", "b", "b"), JarArtifact("a", "b", "a"))) should(be(
        List(JarArtifact("a","b","a"), JarArtifact("a","b","b"))))
      filterArtifacts(List(JarArtifact("a", "b", "a"), JarArtifact("a", "a", "a"))) should(be(
        List(JarArtifact("a","a","a"), JarArtifact("a","b","a"))))
      filterArtifacts(List(JarArtifact("b", "a", "a"), JarArtifact("a", "a", "a"))) should(be(
        List(JarArtifact("a","a","a"), JarArtifact("b","a","a"))))
      filterArtifacts(List(JarArtifact("a", "b", "c"), JarArtifact("d", "e", "f"))) should(be(
        List(JarArtifact("a","b","c"), JarArtifact("d","e","f"))))
      filterArtifacts(List(JarArtifact("d", "e", "f"), JarArtifact("a", "b", "c"))) should(be(
        List(JarArtifact("a","b","c"), JarArtifact("d","e","f"))))
    }
    describe("should rewrite derby") {
      filterArtifacts(JarArtifact("org.apache.derby", "derby", "10.5.3.0") :: Nil) should(be(
      JarArtifact("org.apache.derby", "derby", "10.5.3.0_1") :: Nil
      ))
    }

    describe("should sort after rewrite") {
      filterArtifacts(
        JarArtifact("org.apache.derby", "derby", "10.5.3.0") ::
          JarArtifact("org.apache.derby", "derby", "10.5.3.0_0") :: Nil) should(be(
      JarArtifact("org.apache.derby", "derby", "10.5.3.0_0") ::
          JarArtifact("org.apache.derby", "derby", "10.5.3.0_1") :: Nil
      ))

    }
  }

  type ? = this.type
}