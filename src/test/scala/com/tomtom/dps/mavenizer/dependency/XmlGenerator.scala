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

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Gen._
import xml._

trait XmlGenerator {
  /**Some scalacheck kung fu to generate arbitrary xml with lots of whitespace, returns, etc.*/
  implicit def arbXml(implicit a: Arbitrary[String]): Arbitrary[NodeSeq] =
    Arbitrary {
      val whiteChar: Gen[Char] = Gen.oneOf('\n', '\r', ' ')
      val textChar: Gen[Char] = Gen.frequency((36, alphaNumChar), (3, whiteChar))
      val whiteStr: Gen[String] = for (s <- choose(1, 8); cs <- listOfN(s, whiteChar)) yield cs.mkString
      val textStr: Gen[String] = for (s <- choose(0, 32); cs <- listOfN(s, textChar)) yield cs.mkString
      val genName: Gen[String] = for (s <- choose(1, 8); cs <- listOfN(s, alphaChar)) yield cs.mkString
      val genText: Gen[Text] = for (e <- textStr; pre <- whiteStr; post <- whiteStr) yield new Text(pre + e + post)

      def genElem(sz: Int): Gen[Node] = for {
        n <- Gen.choose(sz / 3, sz / 2)
        c <- Gen.listOfN(n, sizedTree(sz / 2))
        tag <- genName
      } yield Elem(null, tag, Null, TopScope, c: _*)

      def sizedTree(sz: Int): Gen[Node] = if (sz <= 0) {
        genText
      } else {
        Gen.frequency((1, genText), (3, genElem(sz)))
      }

      def sizedRoot(sz: Int): Gen[Node] = genElem(sz)

      for (s <- choose(1, 8); t <- sizedRoot(s)) yield t
    }
}