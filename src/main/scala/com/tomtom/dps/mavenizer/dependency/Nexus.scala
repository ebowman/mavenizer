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

import java.net.{URLEncoder, URL}
import xml.{NodeSeq, XML}

trait NexusComponent {
  /** Interface for implementations which can fill in query details against Nexus. */
  trait Query {
    def toQuery: String
  }

  /** Free text query. */
  case class KeywordQuery(query: String) extends Query {
    def toQuery = "q=" + URLEncoder.encode(query, "UTF-8")
  }

  /** Query against the SHA1 hash of an artifact. */
  case class Sha1Query(sha1: String) extends Query {
    def toQuery = "sha1=" + sha1
  }

  /** Query for a specific class name. */
  case class ClassQuery(classname: String) extends Query {
    def toQuery = "cn=" + classname
  }

  /** Used to construct a query to search for a specific artifact. */
  object StructuredQuery {
    def apply(jar: JarArtifact): StructuredQuery = {
      StructuredQuery(artifactId = Some(jar.artifactId), groupId = Some(jar.groupId), version = Some(jar.version))
    }
  }

  /** Used to construct a query to search for some or all details of an artifact. */
  case class StructuredQuery(artifactId: Option[String] = None,
                             groupId: Option[String] = None,
                             version: Option[String] = None,
                             packaging: Option[String] = None,
                             classifier: Option[String] = None) extends Query {

    /**
     * Generates a query string like "a=xalan&g=xalan" just for those fields that are Some.
     */
    def toQuery = List(
      artifactId.map("a=" + _),
      groupId.map("g=" + _),
      version.map("v=" + _),
      packaging.map("p=" + _),
      classifier.map("c=" + _)
    ).flatten.mkString("&")
  }

  trait Nexus {
    def search(query: Query): List[JarArtifact]
  }

  val nexus: Nexus

  /**
   * See https://repository.sonatype.org/nexus-indexer-lucene-plugin/default/docs/rest.lucene.search.html#GET
   */
  // e.g., http://nlsrvup-nex01:8082/nexus/service/local/lucene/search?
  class NexusImpl(baseUrl: String) extends Nexus {
    /**
     * Performs a query against Nexus, returning a list of jar artifacts that match (may be an empty list).
     */
    def search(query: Query): List[JarArtifact] = {
      val url = new URL(baseUrl + query.toQuery)
      val inputStream = url.openConnection.getInputStream
      try {
        (for (data <- XML.load(inputStream) \ "data"; artifact <- data \ "artifact") yield makeArtifact(artifact)).flatten.toList
      } finally {
        inputStream.close()
      }
    }

    def makeArtifact(artifactXml: NodeSeq): Option[JarArtifact] = {
      for (artifactId <- (artifactXml \ "artifactId").headOption;
           groupId <- (artifactXml \ "groupId").headOption;
           version <- (artifactXml \ "version").headOption) yield
        JarArtifact(groupId.text.trim, artifactId.text.trim, version.text.trim)
    }
  }
}