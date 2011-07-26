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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SHA1Spec extends FeatureSpec with GivenWhenThen {
  type ? = this.type
  val MAX: Int = 64

  feature("SHA1 should generate hashes correctly") {
    scenario("Confirm we can reproduce the results from the command line shasum program") {
      given("A jar file in the project")
      val mailJarInput = getClass.getResourceAsStream("/mail.jar")
      try {
        when("when we compute its hash using SHA1")
        val hash = SHA1(mailJarInput)
        then("the value should match what we computed using the command line")
        assert("63043d27c073e4c1c187119e8b986cb84c9ea8cb" === hash)
      } finally {
        mailJarInput.close()
      }
    }
  }
}