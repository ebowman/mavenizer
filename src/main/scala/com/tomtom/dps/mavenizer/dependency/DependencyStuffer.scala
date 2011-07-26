package com.tomtom.dps.mavenizer.dependency

import com.tomtom.dps.mavenizer.graph.OfbizModuleGraphBuilder
import collection.mutable.Map
import xml.XML
import com.tomtom.dps.mavenizer.{Utils, OFBizHomeDir}
import java.io.{FileInputStream, InputStream, FileOutputStream, File}
import java.util.jar.{JarEntry, JarOutputStream, JarFile}

/**
 * ofbiz-config.xml mentions other things that need to be on the classpath.
 * This object knows how to read it, understand it, and update the jar file to
 * include them.
 */
object DependencyStuffer extends App {

  stuffDeps(OFBizHomeDir(new File(args(0))))

  def stuffDeps(homeDir: OFBizHomeDir) {

    // Generate a list of (jar, dir to add to jar)
    val toAdd: List[(File, File)] = for {
      (dir, node) <- new OfbizModuleGraphBuilder(homeDir).nodes.toList
      components = new File(dir, "ofbiz-component.xml") if components.exists
      classpath <- (XML.loadFile(components) \\ "classpath") if (classpath \ "@type").head.text == "dir"
      jar <- node.jar
    } yield (jar, new File(dir, (classpath \ "@location").head.text))

    // For each jar, compute the set of directories to add to that jar
    val workToDo: Map[File, Set[File]] = toAdd.foldLeft(Map[File, Set[File]]()) {
      case (map, (jar, dir)) =>
        map.get(jar) match {
          case Some(set) => map + (jar -> (set + dir))
          case None => map + (jar -> (Set(dir)))
        }
    }

    for {
      (jarFile: File, add: Set[File]) <- workToDo
    } {
      val newFile = addToJar(jarFile, add)
      val oldFileNewName = new File(jarFile.getParentFile, jarFile.getName + System.currentTimeMillis)
      require(jarFile.renameTo(oldFileNewName), "Failed to rename " + jarFile + " to " + oldFileNewName)
      try {
        require(newFile.renameTo(jarFile), "Could not rename new file " + newFile + " to original name " + jarFile)
        require(oldFileNewName.delete(), "Could not delete original file " + oldFileNewName)
      } catch {
        case e: IllegalArgumentException =>
          require(oldFileNewName.renameTo(jarFile), "Could not rollback from " + e)
      }
    }

    def addToJar(jar: File, filesToAdd: Set[File]): File = {
      val jarFile = new JarFile(jar)
      val tempJar = File.createTempFile(jar.getName, null, jar.getParentFile);
      import collection.JavaConverters._
      import Utils.using
      using(new JarOutputStream(new FileOutputStream(tempJar))) { case jarOut =>
        val buffer = new Array[Byte](64*1024)
        for (entry <- jarFile.entries.asScala) {
          using(jarFile.getInputStream(entry)) { case input: InputStream =>
            jarOut.putNextEntry(entry)
            var bytesRead = 0
            while (bytesRead != -1) {
              bytesRead = input.read(buffer)
              if (bytesRead > 0) {
                jarOut.write(buffer, 0, bytesRead)
              }
            }
          }
        }
        for (dir <- filesToAdd; file <- safe(dir.listFiles) if file.isFile) {
          using(new FileInputStream(file)) { case input: InputStream =>
            jarOut.putNextEntry(new JarEntry(file.getName)) // note: we don't recurse the directory!
            var bytesRead = 0
            while (bytesRead != -1) {
              bytesRead = input.read(buffer)
              if (bytesRead > 0) {
                jarOut.write(buffer, 0, bytesRead)
              }
            }
          }
        }
      }
      tempJar
    }
  }

  def safe[T](array: Array[T]) = if (array == null) Array.empty else array
}