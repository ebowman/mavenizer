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

package com.tomtom.dps.mavenizer.graph

import java.io.File
import xml.XML
import com.tomtom.dps.mavenizer.{OFBizHomeDir, Utils}
import com.tomtom.dps.mavenizer.dependency.{OFBizVersion, JarArtifact}

/**
 * We build a tree structure that identifies the module structure of OFBiz modules
 * as found on disk.
 */

object OfbizModuleGraphBuilder extends App {
  val homeDir = OFBizHomeDir(new File(args(0)))
  val builder = new OfbizModuleGraphBuilder(homeDir)
  println(builder.toDOT)
}

class OfbizModuleGraphBuilder(homeDir: OFBizHomeDir) extends Graph[File] {

  type Node = OfbizModule

  case class OfbizModule(dir: File) extends AbstractNode {
    val xml = XML.loadFile(new File(dir, "build.xml"))
    val LibRegex = """(.*?)(?:/lib|/build/lib)(?:/.*)?""".r
    val HomeDirRegex = """\$\{ofbiz.home.dir\}(.*?)(?:/lib|/build/lib)(?:/.*)?""".r
    val LibDirRegex = """((?:\$\{lib.dir\}|lib).*)""".r
    for {path <- xml \\ "path" if (path \ "@id").text == "local.class.path"
         fileset <- path \ "fileset"
         dirAttr <- fileset \ "@dir"} {
      dirAttr.text match {
        case HomeDirRegex(relPath) => addDependency(this, node(new File(homeDir.dir, relPath).getCanonicalFile))
        case LibRegex(relativePath) => addDependency(this, node(new File(dir, relativePath).getCanonicalFile))
        case LibDirRegex(_) =>
        case wtf => println("WARNING: Couldn't deal with dir = " + wtf); None
      }
    }

    def name = dir.getName

    def artifactId = "ofbiz-" + name

    def groupId = "org.apache.ofbiz" + "." + dir.getParentFile.getName

    def toJarArtifact(version: OFBizVersion) = JarArtifact(groupId, artifactId, version.version)

    lazy val jar: Option[File] = {
      val list = Utils.findFile(dir) {
        case f: File => f.isFile && f.getName.endsWith(".jar") &&
          ((f.getName.startsWith("ofbiz-") && f.getName.contains(f.getParentFile.getParentFile.getParentFile.getName)) || f.getName == "ofbiz.jar") &&
          !f.getName.contains("test")
      }
      require(list.size < 2, "Found multiple possible jars " + list + " for " + this)
      list.headOption
    }
  }

  override def mkNode(key: File) = OfbizModule(key)

  /**
   * Returns a list of modules, which it infers from the file system, looking for files named ofbiz-*.jar, and
   * reverse engineering the module name by working up the directory tree.
   */
  def build(): List[OfbizModule] = {
    val pattern = """ofbiz(:?-.*)?\.jar""".r
    Utils.findFile(homeDir.dir) {
      case (f: File) =>
        // extra complexity because framework/start/build/lib/ofbiz.jar instead of ofbiz-start.jar
        f.isFile && pattern.pattern.matcher(f.getName).matches && f.getParentFile != homeDir.dir
    }.map(_.getParentFile.getParentFile.getParentFile).map(d => node(d)).distinct
  }

  val modules = build()

  assertNoCycles()
  level()
  normalizeDependencies()
}