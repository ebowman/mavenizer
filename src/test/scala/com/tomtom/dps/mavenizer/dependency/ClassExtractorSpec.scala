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
import java.io.{FileOutputStream, File}
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class ClassExtractorSpec extends Spec with ShouldMatchers with PropertyChecks {
  val tmpFile: File = createTmpFile
  val extractor = new ClassExtractor(tmpFile)

  describe("ClassExtractor") {
    describe("a well-known jar file should contain a well known class") {
      extractor.listProperClasses.contains("com.sun.mail.handlers.multipart_mixed") should equal (true)
    }
    describe("a random sample should be as expected") {
      forAll {
        (count: Int) =>
          whenever(count >= 0 && count <= extractor.listProperClasses.size) {
            extractor.random(count).size should equal (count)
          }
      }
    }
  }

  def createTmpFile: File = {
    val tmpFile = File.createTempFile("ClassExtratorSpec", "tmp")
    val output = new FileOutputStream(tmpFile)
    val input = getClass.getResourceAsStream("/mail.jar")
    var done = false
    while (!done) {
      val byte = input.read
      if (byte == -1) {
        done = true
      } else {
        output.write(byte)
      }
    }
    output.close()
    input.close()
    tmpFile
  }

  type ? = this.type
}