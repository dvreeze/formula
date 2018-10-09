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

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path

/**
 * Report, so the "XBRL instance". According to the OIM, the DTS references and facts are unordered,
 * but they are stored here in the order in which they would occur in the corresponding XBRL XML document.
 * Moreover, we need the order of top-level tuple facts, or otherwise we cannot find them using relative
 * paths!
 *
 * This is therefore not a case class, and has no value equality!
 *
 * This class also has some fast query methods, for example, to find facts by concept name.
 *
 * @author Chris de Vreeze
 */
final class Report private (
  dtsReferences: immutable.IndexedSeq[DtsReference],
  topLevelFacts: immutable.IndexedSeq[Fact],
  allFacts: immutable.IndexedSeq[Fact],
  conceptFactMap: Map[EName, immutable.IndexedSeq[Fact]],
  periodSimpleFactMap: Map[PeriodValue, immutable.IndexedSeq[SimpleFact]]) {

  def findAllTopLevelFacts: immutable.IndexedSeq[Fact] = topLevelFacts

  def findAllTopLevelSimpleFacts: immutable.IndexedSeq[SimpleFact] = {
    topLevelFacts.collect { case f: SimpleFact => f }
  }

  def findAllTopLevelNumericSimpleFacts: immutable.IndexedSeq[NumericSimpleFact] = {
    topLevelFacts.collect { case f: NumericSimpleFact => f }
  }

  def findAllTopLevelTupleFacts: immutable.IndexedSeq[TupleFact] = {
    topLevelFacts.collect { case f: TupleFact => f }
  }

  def findAllFacts = allFacts

  def findAllSimpleFacts = allFacts.collect { case f: SimpleFact => f }

  def findAllNumericSimpleFacts = allFacts.collect { case f: NumericSimpleFact => f }

  def findAllTupleFacts = allFacts.collect { case f: TupleFact => f }

  /**
   * Finds all facts having the given concept name. This is a very fast method, using a Map lookup.
   */
  def findFactsByName(conceptName: EName): immutable.IndexedSeq[Fact] = {
    conceptFactMap.getOrElse(conceptName, immutable.IndexedSeq())
  }

  /**
   * Finds all simple facts having the given concept name. This is a very fast method, using a Map lookup.
   */
  def findSimpleFactsByName(conceptName: EName): immutable.IndexedSeq[SimpleFact] = {
    findFactsByName(conceptName).collect { case f: SimpleFact => f }
  }

  /**
   * Finds all numeric simple facts having the given concept name. This is a very fast method, using a Map lookup.
   */
  def findNumericSimpleFactsByName(conceptName: EName): immutable.IndexedSeq[NumericSimpleFact] = {
    findFactsByName(conceptName).collect { case f: NumericSimpleFact => f }
  }

  /**
   * Finds all tuple facts having the given concept name. This is a very fast method, using a Map lookup.
   */
  def findTupleFactsByName(conceptName: EName): immutable.IndexedSeq[TupleFact] = {
    findFactsByName(conceptName).collect { case f: TupleFact => f }
  }

  /**
   * Finds all simple facts having the given period value. This is a very fast method, using a Map lookup.
   */
  def findSimpleFactsByPeriod(periodValue: PeriodValue): immutable.IndexedSeq[SimpleFact] = {
    periodSimpleFactMap.getOrElse(periodValue, immutable.IndexedSeq())
  }

  /**
   * Finds all numeric simple facts having the given period value. This is a very fast method, using a Map lookup.
   */
  def findNumericSimpleFactsByPeriod(periodValue: PeriodValue): immutable.IndexedSeq[NumericSimpleFact] = {
    findSimpleFactsByPeriod(periodValue).collect { case f: NumericSimpleFact => f }
  }

  def aspectUniverse: Set[Aspect] = {
    allFacts.flatMap(_.aspectValueSet.aspects).toSet
  }

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
      sys.error(s"No nested fact found at path $relativePath and 0-based order in parent $orderInParentTuple")
    }
  }

  def findAllDtsReferences: immutable.IndexedSeq[DtsReference] = dtsReferences
}

object Report {

  /**
   * Creates a Report from the given parameters. This is a rather expensive creation method.
   */
  def build(
    dtsReferences: immutable.IndexedSeq[DtsReference],
    topLevelFacts: immutable.IndexedSeq[Fact]): Report = {

    require(topLevelFacts.forall(_.depth == 0), s"Corrupt report, because not all top level facts have depth 0")

    val allFacts: immutable.IndexedSeq[Fact] = {
      topLevelFacts.flatMap {
        case f: SimpleFact => immutable.IndexedSeq(f)
        case f: TupleFact => f +: f.findAllDescendantFacts
      }
    }

    val conceptFactMap = allFacts.groupBy(_.conceptName)

    val allSimpleFacts = allFacts.collect { case f: SimpleFact => f }

    val periodSimpleFactMap = allSimpleFacts.groupBy(_.periodAspectValue.periodValue)

    new Report(
      dtsReferences,
      topLevelFacts,
      allFacts,
      conceptFactMap,
      periodSimpleFactMap)
  }
}
