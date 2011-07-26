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
import xml._

@RunWith(classOf[JUnitRunner])
class XmlNormalizerSpec extends FeatureSpec with GivenWhenThen with PropertyChecks with XmlGenerator {

  import XmlNormalizer.normalize

  feature("XmlNormalizer should normalize XML") {
    scenario("Simple xml text has whitespace removed") {
      given("Some xml")
      val someXml = <hello>world</hello>
      when("text in the xml has prefix and/or postfix whitespace")
      then("the normalized xml should not")
      assert(<hello>world</hello>.toString === normalize(someXml).toString)
    }
    scenario("Normalization should be idempotent") {
      given("Some any xml")
      when("the xml is normalized twice")
      then("it should xml the xml when normalized once")
      forAll {
        (xml: NodeSeq) =>
          assert(normalize(xml).toString === normalize(normalize(xml)).toString)
      }
    }
    scenario("Normalization should be a single line") {
      given("Some any xml")
      when("the xml is normalized")
      then("the result should be on a single line, with no duplicate spaces")
      forAll {
        (xml: NodeSeq) =>
          val norm = normalize(xml).toString()
          assert(norm.split("[\r\n]").size === 1)
          assert("""  """.r.findFirstIn(norm) === None)
      }
    }
  }

  type ? = this.type
}