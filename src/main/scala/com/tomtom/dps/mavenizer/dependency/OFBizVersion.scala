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

import com.tomtom.dps.mavenizer.OFBizHomeDir
import io.Source
import java.io.{FileNotFoundException, File}

object OFBizVersion {
  val VERSION_FILE = "ofbiz.version"
  def apply(homeDir: OFBizHomeDir): OFBizVersion = {
    try {
      val source = Source.fromFile(new File(homeDir.dir, VERSION_FILE), "UTF-8")
      try {
        OFBizVersion(source.getLines().take(1).toList.mkString)
      } finally {
        source.close()
      }
    } catch {
      case e: FileNotFoundException =>
        e.printStackTrace()
        sys.error("Could not open file " + new File(homeDir.dir, VERSION_FILE) +
          "; does it exist? This drives maven versioning")
    }
  }
}

case class OFBizVersion(version: String)