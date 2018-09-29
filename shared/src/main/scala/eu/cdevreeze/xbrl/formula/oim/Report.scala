/*
 * Copyright 2011-2018 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.xbrl.formula.oim

import scala.collection.immutable

import eu.cdevreeze.yaidom.core.Path

/**
 * Report, so the "XBRL instance". According to the OIM, the DTS references and facts are unordered,
 * but they are stored here in the order in which they would occur in the corresponding XBRL XML document.
 * Moreover, we need the order of top-level tuple facts, or otherwise we cannot find them using relative
 * paths!
 *
 * This is therefore not a case class, and has no value equality!
 *
 * @author Chris de Vreeze
 */
final class Report(
  val dtsReferences: immutable.IndexedSeq[DtsReference],
  val topLevelFacts: immutable.IndexedSeq[Fact]) {

  /**
   * First finds the top-level tuple fact at the first entry of the given relative path, and then on that tuple
   * calls method findDescendantFact with the remainder of the path (and the same order) as parameters.
   * Returns None if no result fact is found. Also returns None if the relative path is empty, because then
   * we would not be searching for nested facts.
   */
  def findNestedFact(relativePath: Path, orderInParentTuple: Int): Option[Fact] = {
    if (relativePath.isEmpty) {
      // Top-level fact, so not a nested fact.
      None
    } else {
      val firstPathEntry = relativePath.firstEntry
      val topLevelFactName = firstPathEntry.elementName
      val topLevelFactOption =
        topLevelFacts.filter(_.conceptName == topLevelFactName).drop(firstPathEntry.index).headOption
      val topLevelTupleOption = topLevelFactOption.collect { case f: TupleFact => f }

      topLevelTupleOption.flatMap(_.findDescendantFact(relativePath.withoutFirstEntry, orderInParentTuple))
    }
  }

  def getNestedFact(relativePath: Path, orderInParentTuple: Int): Fact = {
    findNestedFact(relativePath, orderInParentTuple).getOrElse {
      sys.error(s"No fact found at path $relativePath and 0-based order in parent $orderInParentTuple")
    }
  }
}
