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
import java.io.{PrintWriter, OutputStreamWriter, FileOutputStream, File}
import xml.PrettyPrinter
import java.util.Date
import com.tomtom.dps.mavenizer.config.Config

/**
 * This creates a root pom which will generate a tarball of the complete ofbiz directory mod jars and .class files
 */
object RootPom {

  def writeRootPom(version: OFBizVersion, homeDir: OFBizHomeDir) {
      val pom = <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">
        <modelVersion>4.0.0</modelVersion>
        <groupId>org.apache.ofbiz.framework</groupId>
        <artifactId>ofbiz-super</artifactId>
        <version>{version.version}</version>
        <name>${{project.artifactId}}</name>
        <description>Auto-generated OFBiz pom -- {new Date}</description>
        <inceptionYear>2011</inceptionYear>
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
        <build>
            <plugins>
                <plugin>
                    <groupId>org.apache.maven.plugins</groupId>
                    <artifactId>maven-assembly-plugin</artifactId>
                    <version>2.2.1</version>
                    <configuration>
                        <descriptors>
                            <descriptor>src/main/assembly/src.xml</descriptor>
                        </descriptors>
                    </configuration>
                </plugin>
            </plugins>
        </build>
      </project>

    import Utils.using

    using(new PrintWriter(new OutputStreamWriter(new FileOutputStream(new File(homeDir.dir, "pom.xml")), "UTF-8"))) {
      case writer =>
        writer.print(new PrettyPrinter(80, 2).formatNodes(pom))
    }
  }
}