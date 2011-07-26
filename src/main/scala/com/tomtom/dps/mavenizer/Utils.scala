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

package com.tomtom.dps.mavenizer

import java.io.File
import collection.mutable.ListBuffer

object OFBizHomeDir {
  implicit def ofbizToFile(home: OFBizHomeDir): File = home.dir
}
case class OFBizHomeDir(dir: File) {
  require(dir.exists(), "OFBiz home directory " + dir + " not found")
  require(dir.isDirectory, dir + " is not a directory")
  require(new File(dir, "ofbiz.jar").exists, "ofbiz.jar not found; did you build ofbiz first?")
}

object Utils {
  /**
   * Finds every file that matches the given pattern.
   */
  def findFile(dir: File)(matcher: File => Boolean): List[File] = {
    val builder = new ListBuffer[File]
    def recurse(file: File) {
      if (matcher(file)) {
        builder += file
      }
      if (file.isDirectory) {
        file.listFiles.foreach {
          f => recurse(f)
        }
      }
    }
    recurse(dir)
    builder.toList
  }

  def using[T <: { def close() }, R](i: T)(f: T => R): R = {
    try {
      f(i)
    } finally {
      i.close()
    }
  }
}