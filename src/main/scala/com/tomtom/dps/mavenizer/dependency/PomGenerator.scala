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

import com.tomtom.dps.mavenizer.graph.OfbizModuleGraphBuilder
import com.tomtom.dps.mavenizer.{Utils, OFBizHomeDir}
import xml.{PrettyPrinter, NodeSeq}
import java.util.Date
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.io._
import com.tomtom.dps.mavenizer.config.Config
import scala.Predef._

object PomGenerator extends App with DependencyFinderComponent with NexusComponent {

  require(args.size > 0, "Usage: PomGenerator [path to config file]")

  import OFBizHomeDir._
  Config.loadFile(new File(args(0)))
  import Config.config

  val nexus = new NexusImpl(config.stringOpt("nexus.searchUrl").getOrElse("http://nlsrvup-nex01:8082/nexus/service/local/lucene/search?"))
  val homeDir = OFBizHomeDir(config.file("ofbiz.home"))

  val skipNexusCheck = args.find(_ == "--skip-nexus-check") != None

  // get a list of all the jar files in the given directory
  val jarFiles = homeDir.listFiles.filter(
    f => f.isFile && f.getName.endsWith(".jar") && !f.getName.contains("ofbiz")).toList

  override val dependencyFinder = new DependencyFinderImpl

  // try to resolve this list of jars into a list of JarDependencies.
  // If we find there are jars that do not map to known artifacts in Nexus,
  // this returns a list of commands to run (in some cases a list of possible commands, with further
  // manual input required to decide which of several options to run).
  if (!skipNexusCheck) {
    dependencyFinder.resolveDependencies(jarFiles) match {
      case Left(deployCommands) =>
        // We didn't find everything we needed; print out some commands that could be run from
        // a shell, and exit indicating an error
        deployCommands.foreach(c => {
          println(); println(c.message); c.commands.foreach(println)
        })
        sys.exit(1)
      case Right(artifacts: List[JarArtifact]) =>
        // We found everything we needed in Nexus
        println("All artifacts found; proceeding to extract module information")
    }
  }

  var version = OFBizVersion(homeDir)
  println("version = " + version)
  val builder = new OfbizModuleGraphBuilder(homeDir)

  for {
    (dir: File, module) <- builder.nodes;
    thirdPartyJars <- dependencyFinder.resolveDependencies(Utils.findFile(new File(dir, "lib")) {
      case f: File => f.isFile && f.getName.endsWith(".jar")
    }).right} {

      println("# Considering " + module)
      val dependencies = builder.dependencies(module).map(_.toJarArtifact(version)).toList ::: thirdPartyJars
      val pom = createPom(module.groupId, module.artifactId, version.version, dependencies)
      val writer = new PrintWriter(new FileWriter(new File(dir, "pom.xml")))
      try {
        println("# Writing " + new File(dir, "pom.xml"))
        writer.print(new PrettyPrinter(1024, 2).formatNodes(XmlNormalizer.normalize(pom)))
      } finally {
        writer.close()
      }
      Utils.findFile(new File(dir,"build/lib")) {
        // ofbiz-start is special; the jar is named "ofbiz.jar" not "ofbiz-start.jar" :(
        case f: File => f.isFile && !f.getName.contains("test") &&
          ((f.getName.startsWith("ofbiz-") && f.getName.endsWith(".jar")) || f.getName == "ofbiz.jar")
      } match {
        case jar :: Nil =>
          println(DeployCommand(jar, new File(dir, "pom.xml")).command)
          generateSourceJar(dir) foreach { jarFile =>
            println(DeployCommand(jarFile, new File(dir, "pom.xml"), classifier=Some("sources")).command)
          }
        case Nil => println("No jar found for " + dir)
        case jars => sys.error("Could not resolve a unique jar in " + dir + ": " + jars)
      }
  }

  RootPom.writeRootPom(version, homeDir)
  println("cd " + homeDir.dir.getAbsolutePath)
  println("mvn clean assembly:single")
  println(DeployCommand(
    new File(new File(homeDir.dir, "target"), "ofbiz-super-" + version.version + "-distribution.tar.gz"),
    new File(homeDir.dir, "pom.xml"),
    packaging = "targz").command)

  def createPom(groupId: String, artifactId: String, version: String, dependencies: List[JarArtifact]): NodeSeq = {
    <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      <modelVersion>4.0.0</modelVersion>
      <groupId>
        {groupId}
      </groupId>
      <artifactId>
        {artifactId}
      </artifactId>
      <version>
        {version}
      </version>
      <name>${{project.artifactId}}</name>
      <description>Auto-generated OFBiz pom -- {new Date().toString}</description>
      <inceptionYear>2011</inceptionYear>

      {if (Config.config.bool("nexus.includeRepositories", true)) {
      <repositories>
        <repository>
          <id>{Config.config.string("nexus.repositoryId")}</id>
          <url>{Config.config.string("nexus.repositoryUrl")}</url>
          <releases>
            <enabled>true</enabled>
          </releases>
          <snapshots>
            <enabled>true</enabled>
            <updatePolicy>always</updatePolicy>
          </snapshots>
        </repository>
      </repositories>
      <dependencies>
        {dependencies.map(_.toXml)}
      </dependencies>
    }}
    </project>
  }

  def generateSourceJar(dir: File): Option[File] = {
    val jarFile = new File(dir, "src.jar")
    val srcDir = new File(dir, "src")
    if (!srcDir.exists || !srcDir.isDirectory) {
      println("# warning: no src dir found in " + dir)
      None
    } else {
      val srcDirPath = srcDir.getCanonicalPath
      val files = Utils.findFile(srcDir)(f => f.isFile).
        map(_.getCanonicalPath).
        filter(_ startsWith srcDirPath).
        map(p => p.substring(srcDirPath.size + 1))
      val zipOut = new ZipOutputStream(new FileOutputStream(jarFile))
      try {
        for (file <- files) {
          val zipEntry = new ZipEntry(file)
          zipOut.putNextEntry(zipEntry)
          val input = new FileInputStream(new File(srcDir, file))
          try {
            val buffer = new Array[Byte](64*1024)
            var done = false
            while (!done) {
              val bytesRead = input.read(buffer)
              if (bytesRead == -1) {
                zipOut.closeEntry()
                done = true
              } else {
                zipOut.write(buffer, 0, bytesRead)
              }
            }
          }
          finally {
            input.close()
          }
        }
        Some(jarFile)
      } finally {
        zipOut.close()
      }
    }
  }
}

