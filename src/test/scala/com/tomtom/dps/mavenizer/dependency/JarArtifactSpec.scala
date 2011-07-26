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

@RunWith(classOf[JUnitRunner])
class JarArtifactSpec extends FeatureSpec with GivenWhenThen with PropertyChecks {
  type ? = this.type
  val MAX: Int = 64

  feature("JarArtifact should convert itself to XML predictably") {
    scenario("a known artifactId, groupId and version") {
      given("An artifactId, a groupId, and a version")
      when("a JarArtifact is created from them")
      then("the dependency xml would be predictable")
      forAll {
        (artifactId: String, groupId: String, version: String) =>
          val art = JarArtifact(artifactId = artifactId, groupId = groupId, version = version)
          import XmlNormalizer.normalize
          assert(normalize(<dependency>
            <groupId>
              {groupId}
            </groupId>
            <artifactId>
              {artifactId}
            </artifactId>
            <version>
              {version}
            </version>
          </dependency>).toString() === normalize(art.toXml).toString())
      }
    }
  }
}