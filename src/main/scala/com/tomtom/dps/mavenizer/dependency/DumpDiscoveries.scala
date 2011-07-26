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

import com.tomtom.dps.mavenizer.{Utils, OFBizHomeDir}
import java.io.File
import com.tomtom.dps.mavenizer.config.Config

/**
 * Generates a bunch of test code based on what jars it actually finds, and how it sniffs out their versions.
 */
object DumpDiscoveries extends App with DependencyFinderComponent with NexusComponent {

  import OFBizHomeDir._
  require(args.size > 0, "Usage: DumpDiscoveries [path to config file]")
  Config.loadFile(new File(args(0)))
  import Config.config

  val nexus = new NexusImpl(config.stringOpt("nexus.searchUrl").getOrElse("http://nlsrvup-nex01:8082/nexus/service/local/lucene/search?"))
  val homeDir = OFBizHomeDir(config.file("ofbiz.home"))
  override val dependencyFinder = new DependencyFinderImpl
  Utils.findFile(homeDir) {
    case f: File => f.isFile && f.getName.endsWith(".jar")
  }.foreach {
    f =>
      val guess = dependencyFinder.guessArtifactDetails(f)
      val version = guess._2 match {
        case Some(v) => "Some(\"" + v + "\")"
        case None => "None"
      }
      println("guessArtifactDetails(\"" + f.getName + "\") should equal ((\"" + guess._1 + "\", " + version + "))")
  }
}
