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

import java.security.MessageDigest
import java.io.{FileInputStream, File, InputStream}

object SHA1 {

    def apply(file: File): String = {
        val input = new FileInputStream(file)
        try {
            SHA1(input)
        } finally {
            input.close()
        }
    }

    def apply(input: InputStream): String = {
        var md: MessageDigest = null
        md = MessageDigest.getInstance("SHA-1")
        val buffer = new Array[Byte](16384)
        var done = false
        while (!done) {
            input.read(buffer) match {
                case -1 => done = true
                case bytesRead if bytesRead > 0 =>
                    md.update(buffer, 0, bytesRead)
                case _ => // nothing read, do nothing
            }
        }
        md.digest().map("%02x".format(_)).mkString
    }
}


