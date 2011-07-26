package com.tomtom.dps.mavenizer.graph

import java.io.File
import xml.XML
import util.matching.Regex
import collection.mutable.ListBuffer

/**
 * We build a tree structure that identifies the module structure of OFBiz modules
 * as found on disk.
 */


object Builder extends Graph[File] {

  type Node = Module

  var homeDir: File = _

  case class Module(dir: File) extends AbstractNode {
    println("New module" + dir)
    val xml = XML.loadFile(new File(dir, "build.xml"))
    val LibRegex = """(.*?)(?:/lib|/build/lib)(?:/.*)?""".r
    val HomeDirRegex = """\$\{ofbiz.home.dir\}(.*?)(?:/lib|/build/lib)(?:/.*)?""".r
    val LibDirRegex = """((?:\$\{lib.dir\}|lib).*)""".r
    for {path <- xml \\ "path" if (path \ "@id").text == "local.class.path"
         fileset <- path \ "fileset"; dirAttr <- fileset \ "@dir"} {
      dirAttr.text match {
        case HomeDirRegex(relPath) => addDependency(this, node(new File(homeDir, relPath).getCanonicalFile))
        case LibRegex(relativePath) => addDependency(this, node(new File(dir, relativePath).getCanonicalFile))
        case LibDirRegex(_) =>
        case wtf => println("WARNING: Couldn't deal with dir = " + wtf); None
      }
    }

    def name = dir.getName

    def artifactId = "ofbiz-" + name

    def groupId = "org.apache.ofbiz" + "." + dir.getParentFile.getName

    override def toString = name + "_" + level
  }

  def mkNode(key: File) = Module(key)

  def main(args: Array[String]) {

    // build all the modules for a given directory
    homeDir = new File("/Users/ebowman/tomtom/dps/ofbiz")
    val modules = build()

    assertNoCycles()
    println("The dependency graph is acyclic.")

    modules.foreach {
      module: Module =>
        dependencies(module) foreach {
          dependency: Module =>
            println(module.name + " -> " + dependency.name)
        }
    }

    println("About to level")
    level()
    println(toDOT)
    println("About to normalize")
    normalizeDependencies()
    println(toDOT)
    println("--------------------------------------")
    modules.foreach {
      module: Module =>
        dependencies(module) foreach {
          dependency: Module =>
            println(module.name + " (" + module.level + ") -> " + dependency.name)
        }
    }
  }

  def build(): List[Module] = {
    findFile(homeDir, """ofbiz-.*\.jar""".r).map(
      _.getParentFile.getParentFile.getParentFile).map(d => node(d)).distinct
  }

  /**
   * Finds every file that matches the given pattern.
   */
  def findFile(dir: File, pattern: Regex, results: List[File] = Nil): List[File] = {
    val builder = new ListBuffer[File]

    def recurse(file: File) {
      if (file.isDirectory) {
        file.listFiles.foreach {
          f => recurse(f)
        }
      } else {
        if (pattern.pattern.matcher(file.getName).matches) {
          builder += file
        }
      }
    }
    recurse(dir)
    builder.toList
  }
}