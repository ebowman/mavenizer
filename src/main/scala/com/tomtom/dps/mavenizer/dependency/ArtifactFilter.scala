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

/**
 * This class is a place to put special, unpleasant logic.  For one thing, in the case
 * where there are multiple possibilities, we want to make sure we always pick the same one.
 * Also, it turns out, there are some jars in OFBiz which do not map to nice maven versions
 * (in particular, see ah: http://www.mail-archive.com/users%40maven.apache.org/msg104547.html).
 * It is here that we have an opportunity to fix that.
 */
object ArtifactFilter {

  val rewrites = Map(
    JarArtifact("org.apache.derby", "derby", "10.5.3.0") ->
      JarArtifact("org.apache.derby", "derby", "10.5.3.0_1"))

  def filterArtifacts(artifacts: List[JarArtifact]): List[JarArtifact] = {
    artifacts map { case artifact => rewrites.get(artifact).getOrElse(artifact) } sortWith(_ < _)
  }
}