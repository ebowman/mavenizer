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
import java.util.Date
import java.text.SimpleDateFormat
import com.tomtom.dps.mavenizer.config.Config

trait DependencyFinderComponent {

  this: NexusComponent =>

  /**
   * Interface for an object that manifests one or more commands, and a description of what they do.
   */
  trait DeployCommand {
    def message: String

    def commands: List[String]
  }

  trait DependencyFinder {
    def resolveDependencies(jars: List[File]): Either[List[DeployCommand], List[JarArtifact]]

    def guessArtifactDetails(fileName: File): (String, Option[String])
  }

  val dependencyFinder: DependencyFinder

  /**
   * A DeployCommand for deploying a single jar into Nexus against a single JarArtifact
   */
  case class ConcreteDeployCommand(jarFile: File, artifact: JarArtifact) extends DeployCommand {

    def message = "\n# For " + jarFile + " there is a single possibility"

    def commands: List[String] = {
      "mvn deploy:deploy-file " + List(
        "artifactId" -> artifact.artifactId,
        "groupId" -> artifact.groupId,
        "version" -> artifact.version,
        "file" -> jarFile,
        "url" -> Config.config.string("nexus.deployUrl"),
        "repositoryId" -> Config.config.string("nexus.repositoryId"),
        "packaging" -> "jar").
        map(i => "-D" + i._1 + "=" + i._2).mkString(" ") :: Nil
    }
  }

  /**
   * A DeployCommand which manifests the fact we aren't sure which deploy command is best
   * (so, severaly possible JarArtifact to describe a given jar).
   */
  case class DegenerateDeployCommand(jarFile: File, artifact: List[JarArtifact]) extends DeployCommand {
    def message = "# For " + jarFile + " there are " + artifact.size + " possibilities"

    def commands = artifact.map(ConcreteDeployCommand(jarFile, _).commands).flatten
  }


  /**
   * Object presenting an interface to determine, for a list of jars, the JarArtifacts that can be used
   * to populate a <dependencies> blob in a pom, or the commands to execute in order to create artifacts
   * in Nexus so that the next time the program is run, it can populate a <dependencies> blob.
   */
  class DependencyFinderImpl extends DependencyFinder {

    /**How many random classes to sample to try to find a close match. */
    val SAMPLE_SIZE = 10

    /**
     * Main entrypoint: for these jars, compute either the list of JarArtifacts so a pom can be created,
     * or generate the list of commands that should be run to put missing JarArtifacts in Nexus.
     */
    def resolveDependencies(jars: List[File]): Either[List[DeployCommand], List[JarArtifact]] = {

      // Each jar results in either a JarArtifact (because we found this jar in maven), or a DeployCommand
      // (which describes how to put this jar in maven).  So we end up with a list of eithers.
      import ArtifactFilter.filterArtifacts
      val maybe: List[Either[DeployCommand, JarArtifact]] =
        for (jar <- jars) yield {
          filterArtifacts(nexus.search(Sha1Query(sha1 = SHA1(jar)))) match {
            case Nil => Left(generateUnknownJarCommand(jar))
            case unique :: _ => Right(unique) // if SHA1 is degenerate, just take the first one
          }
        }

      // Now, if there are ANY DeployCommands present, we don't want to proceed.  So we pull out the DeployCommands
      // from the maybe list, and if it's empty then we return a Right containing the list of artifacts. Otherwise we
      // return a Left containing the list of deploy commands.
      (for (Left(command) <- maybe) yield command) match {
        case Nil => Right(for (Right(artifact) <- maybe) yield artifact)
        case commandList => Left(commandList)
      }
    }

    private def generateUnknownJarCommand(jar: File): DeployCommand = {

      // Take a random sample of classes, and for each class come up with a list of artifacts that
      // contain that class.  Then compute a score for each artifact, which is the number of these classes
      // that artifact contained.  This is flawed in theory but nice in practice; in theory we should check
      // every class, but that's a bit slow.
      val classes = new ClassExtractor(jar).random(SAMPLE_SIZE)

      // Generate a map from each class, to the set of artifacts that contain it.
      val toScore: Map[String, Set[JarArtifact]] = classes.map(c => (c -> nexus.search(ClassQuery(c)).toSet)).toMap

      // For each artifact, compute how many of the classes mentioned it contained. We favor artifacts that contained
      // more (in many cases the score is the same for all, though, so we have to deal with that too).
      val scores = new collection.mutable.HashMap[JarArtifact, Int]() {
        override def default(key: JarArtifact) = 0
      }
      for (clazz <- toScore.keys; artifact <- toScore(clazz)) {
        scores(artifact) = scores(artifact) + 1
      }

      if (scores.size == 0) {
        // There was no artifact that had any of the classes in this jar; in this
        // case we have to create an entirely new artifact for nexus; we use some
        // heuristics to guess a good groupId/artifactId/version for it.

        var versionModifier: Option[Int] = None // if we guess a version that already exists, we use this to tweak it
        while (true) {
          // try to guess the name & version, generate a reasonable JarArtifact,
          // then check to see does that artifact already exist in Nexus. If it does,
          // keep tacking on "-dps-1", "-dps-2", etc. until we find a JarArtifact
          // that does not exist in Nexus.  So we get nice "-1" is why we keep subtracting
          // one from versionModifier each time we get a version that already exists in nexus.
          val (name, maybeVersion) = guessArtifactDetails(jar)
          val version = synthesizeVersion(maybeVersion, versionModifier)
          val artifact = JarArtifact(name, name, version) // use the name as the groupId as well; got a better idea?

          // If this artifact does not exist in nexus, then we're done, and we can supply a command.
          // If it does exist, then keep tweaking the version as mentioned above until it doesn't exist.
          nexus.search(StructuredQuery(artifact)) match {
            case Nil => return ConcreteDeployCommand(jar, artifact)
            case _ => versionModifier = Some(versionModifier.getOrElse(0) - 1)
          }
        }

        // we loop above until we come up with an artifact that's not in Nexus. That has got to terminate eventually.
        sys.error("Not reachable")
      } else {
        // There is at least one artifact that contains at least some of the classes in this jar

        // Filter out the highest score, and all the artifacts that had that score
        val best: (Int, List[JarArtifact]) = scores.foldLeft((Int.MinValue, Nil: List[JarArtifact])) {
          case ((maxScore, artifacts), (artifact, score)) =>
            if (score == maxScore) {
              (maxScore, artifact :: artifacts)
            } else if (score > maxScore) {
              (score, artifact :: Nil)
            } else {
              (maxScore, artifacts)
            }
        }

        // Filter down best to just a set of (groupId, artifactId) pairs.  If we are lucky, this is just one.
        // We use a List here instead of a Set because we don't want to lose the ordering we got back from Nexus,
        // which is probably ordered in a reasonable way.
        val versionLess: List[(String, String)] =
          best._2.map(jarArtifact => (jarArtifact.groupId, jarArtifact.artifactId)).distinct


        var versionModifier: Option[Int] = None // if we guess a version that already exists, we use this to tweak it

        // We loop until we have synthesized an artifact that does not already exist in Nexus. This is guaranteed
        // to terminate before the heat-death of the universe, and probably sooner.
        while (true) {
          // Come up with a version for our artifact; we keep tweaking this using the versonModifier in order
          // to go through the sequence "version-dps-1", "version-dps-2" etc. until we find a version that is
          // not already in nexus.
          val (_, maybeVersion) = guessArtifactDetails(jar)
          val version = synthesizeVersion(maybeVersion, versionModifier)

          // For all the "best" (groupId, artifactId) pairs, create a list of corresponding JarArtifacts
          // that manifest the current version we are hoping to use.
          val artifacts: List[JarArtifact] =
            versionLess.map(ga => JarArtifact(groupId = ga._1, artifactId = ga._2, version = version))

          // Figure out which, if any, of these artifacts are already in Nexus
          val found: List[List[JarArtifact]] = artifacts.map(artifact => nexus.search(StructuredQuery(artifact)))

          // zip together each artifact with its list of found matching artifacts in nexus,
          // and keep only those which are NOT in nexus (in other words, the list of found matching
          // artifacts is Nil.
          // If there is only 1, then we know exactly which command to execute.
          // If there is more than one, then there is a list of possible commands to execute, and the operator should pick
          // If there none, then this version is already taken, so we tweak versionModifier and try again.
          artifacts.zip(found).filter(_._2 == Nil).map(_._1) match {
            case unique :: Nil =>
              return ConcreteDeployCommand(jar, unique)
            case multiple@_ :: _ =>
              return DegenerateDeployCommand(jar, multiple)
            case Nil =>
              versionModifier = Some(versionModifier.getOrElse(0) - 1)
          }
        }
        sys.error("Not reachable")
      }
    }

    // various regexes we've discovered to extract artifact names and versions from jar file names
    val UglyVersionMatcher = """([a-zA-Z-]+)-([0-9]+-.*)\.jar""".r // match webslinger-base-invoker-20091211-3897-7ab22baea4b6.jar
    val VersionMatcher = """([a-zA-Z_-]+)[-\.]([0-9_\.]+)\.jar""".r // match lots of things
    val RCMatcher = """([a-zA-Z0-9]+)-([0-9\.a-zA-Z-_]+).jar""".r // match things like ical4j-1.0-rc2.jar
    val NoVersion = """([a-zA-Z0-9]+).jar""".r // match names w/o a version

    /*
    "org.apache.xml.resolver_1.2.0.v200902170519.jar"
     */
    val ResolverFunky = """([a-zA-Z\.]+)_([0-9\.v]+).jar""".r

    /**
     * crosstabcoreapi.jar
     */
    val Vanilla = """([a-zA-Z-_]+)\.jar""".r

    /**
     * xpp3_min-1.1.4c.jar
     */
    val XppMatcher = """([a-zA-Z0-9_]+)-([0-9\.a-z]+)\.jar""".r

    /**
     * org.eclipse.birt.chart.reportitem_2.5.1.v20090902a.jar
     */
    val EclipseMatcher = """([a-z\.0-9]+)_(.*?)\.jar""".r

    /**
     * jpos18-controls.jar
     */
    val JposMatcher = """([a-zA-Z0-9]+-[a-zA-Z]+)\.jar""".r

    /**
     * "ws-commons-java5-1.0.1.jar"
     */
    val Java5Matcher = """([a-z0-9-]+)-([0-9\.]+)\.jar""".r

    /**
     * geronimo-activation_1.0.2_spec-1.0.jar
     */
    val GeronimoMatcher = """([a-z0-9-]+)_(([0-9\.]+)_([a-z0-9\.-]+))\.jar""".r

    /**
     * tomcat-6.0.26-annotations-api.ja
     */
    val TomcatMatcher = """tomcat-([0-9\.]+)-(.*)\.jar""".r

    /**
     * commons-dbcp-1.3-20091113-r835956.jar
     */
    val CommonsMatcher = """([a-z-]+)-([0-9]+.*)\.jar""".r

    /**
     * Given a filename, try to guess the artifactId and version.
     */
    def guessArtifactDetails(fileName: File): (String, Option[String]) = {
      // oops, i broke the compiler.
      try {
        fileName.getName match {
          case GeronimoMatcher(name, version, _, _) => (name, Some(version))
          case Java5Matcher(name, version) => (name, Some(version))
          case JposMatcher(name) => (name, None)
          case Vanilla(name) => (name, None)
          case UglyVersionMatcher(name, version) => (name, Some(version))
          case VersionMatcher(name, version) => (name, Some(version))
        }
      } catch {
        case e: MatchError =>
          fileName.getName match {
            case TomcatMatcher(version, name) => ("tomcat-" + name, Some(version))
            case CommonsMatcher(name, version) => (name, Some(version))
            case RCMatcher(name, version) => (name, Some(version))
            case NoVersion(name) => (name, None)
            case ResolverFunky(name, version) => (name, Some(version))
            case XppMatcher(name, version) => (name, Some(version))
            case EclipseMatcher(name, version) => (name, Some(version))
          }
      }
    }

    /**
     * Given a possible version, and a possible way to perturb it, synthesize a version string.
     */
    private def synthesizeVersion(version: Option[String], versionPerturbation: Option[Int]): String = {
      def makeDate = new SimpleDateFormat("yyyyMMdd").format(new Date)
      version match {
        case Some(v) if v endsWith "-SNAPSHOT" =>
          v.replaceAll("SNAPSHOT", makeDate) + "-dps" + versionPerturbation.getOrElse("")
        case Some(v) => v + versionPerturbation.getOrElse("")
        case None => makeDate + "-dps" + versionPerturbation.getOrElse("")
      }
    }
  }

}
