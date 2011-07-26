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

import java.io.File
import java.util.zip.ZipFile
import collection.JavaConverters._

/**
 * Utility class to extract the list of classes in a jar, either in their entirety, or as a random sample.
 * Ignores inner classes (those with a "$" in the name)
 */
class ClassExtractor(jar: File) {
  /**
   * Given a path to a jar file, returns a list of all classes in that jar that don't have a "$" in the name.
   */
  lazy val listProperClasses: List[String] = {
    val zipFile: ZipFile = new ZipFile(jar)
    try {
      zipFile.entries.asScala.map(_.getName).filter(_.endsWith(".class")).filterNot(_.contains("$")).
        map(_.dropRight(".class".length)).map(_.replaceAll("/",".")).toList
    } finally {
      zipFile.close()
    }
  }

  /**
   * Returns a random sample of the classes in a jar, of count as given.
   */
  def random(count: Int): List[String] = util.Random.shuffle(listProperClasses).take(count)
}