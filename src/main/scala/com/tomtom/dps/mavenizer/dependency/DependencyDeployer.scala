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

import java.io._
import com.tomtom.dps.mavenizer.config.Config
import com.tomtom.dps.mavenizer.{Utils, OFBizHomeDir}
import com.tomtom.dps.mavenizer.OFBizHomeDir._

object DependencyDeployer extends App with DependencyFinderComponent with NexusComponent {

  import OFBizHomeDir._
  require(args.size > 0, "Usage: DumpDiscoveries [path to config file]")
  Config.loadFile(new File(args(0)))
  import Config.config

  val nexus = new NexusImpl(config.stringOpt("nexus.searchUrl").
    getOrElse("http://nlsrvup-nex01:8082/nexus/service/local/lucene/search?"))
  val homeDir = OFBizHomeDir(config.file("ofbiz.home"))

  // get a list of all the jar files in the given directory
  val jarFiles = Utils.findFile(homeDir) {
    case f: File => f.isFile && f.getName.endsWith(".jar") && !f.getName.contains("ofbiz") && f.getName != "src.jar"
  }

  override val dependencyFinder = new DependencyFinderImpl

  // try to resolve this list of jars into a list of JarDependencies.
  // If we find there are jars that do not map to known artifacts in Nexus,
  // this returns a list of commands to run (in some cases a list of possible commands, with further
  // manual input required to decide which of several options to run).
  dependencyFinder.resolveDependencies(jarFiles) match {
    case Left(deployCommands) =>
      // We didn't find everything we needed; print out some commands that could be run from
      // a shell, and exit indicating an error
      deployCommands.foreach(c => {
        println();
        println(c.message);
        c.commands.foreach(println)
      })
      sys.exit(1)
    case Right(artifacts: List[JarArtifact]) =>
      // We found everything we needed in Nexus
      println("All artifacts found; proceed to next step")
  }
}

