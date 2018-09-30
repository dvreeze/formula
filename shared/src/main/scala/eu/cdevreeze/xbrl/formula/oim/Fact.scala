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
 * Fact, so either a simple fact or a tuple fact.
 *
 * @author Chris de Vreeze
 */
sealed trait Fact {

  def idOption: Option[String]

  /**
   * The aspect values of this fact. No aspect values in this set may have the same aspect. The aspects
   * must be applicable to the kind of fact.
   */
  def aspectValues: Set[AspectValue]

  final def conceptName: EName = {
    conceptAspectValue.conceptName
  }

  final def conceptAspectValue: ConceptAspectValue = {
    aspectValues.collect { case av: ConceptAspectValue => av }
      .headOption.getOrElse(sys.error(s"Missing concept aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def tupleParentAspectValue: TupleParentAspectValue = {
    aspectValues.collect { case av: TupleParentAspectValue => av }
      .headOption.getOrElse(sys.error(s"Missing tuple parent aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def tupleOrderAspectValue: TupleOrderAspectValue = {
    aspectValues.collect { case av: TupleOrderAspectValue => av }
      .headOption.getOrElse(sys.error(s"Missing tuple order aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  // TODO Footnotes
}

sealed trait SimpleFact extends Fact {

  def factValue: SimpleFactValue

  final def entityAspectValue: EntityAspectValue = {
    aspectValues.collect { case av: EntityAspectValue => av }
      .headOption.getOrElse(sys.error(s"Missing entity aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def periodAspectValue: PeriodAspectValue = {
    aspectValues.collect { case av: PeriodAspectValue => av }
      .headOption.getOrElse(sys.error(s"Missing period aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def languageAspectValue: LanguageAspectValue = {
    aspectValues.collect { case av: LanguageAspectValue => av }
      .headOption.getOrElse(sys.error(s"Missing language aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def dimensionAspectValues: Set[DimensionAspectValue] = {
    aspectValues.collect { case av: DimensionAspectValue => av }
  }

  final def explicitDimensionAspectValues: Set[ExplicitDimensionAspectValue] = {
    aspectValues.collect { case av: ExplicitDimensionAspectValue => av }
  }

  final def typedDimensionAspectValues: Set[TypedDimensionAspectValue] = {
    aspectValues.collect { case av: TypedDimensionAspectValue => av }
  }
}

final case class NonNumericSimpleFact(
  idOption: Option[String],
  aspectValues: Set[AspectValue],
  factValue: SimpleFactValue) extends SimpleFact

final case class NumericSimpleFact(
  idOption: Option[String],
  aspectValues: Set[AspectValue],
  factValue: SimpleFactValue,
  accuracy: Accuracy) extends SimpleFact {

  final def unitAspectValue: UnitAspectValue = {
    aspectValues.collect { case av: UnitAspectValue => av }
      .headOption.getOrElse(sys.error(s"Missing unit aspect in fact with ID ${idOption.getOrElse("")}"))
  }
}

/**
 * Tuple fact. The child facts are contained in this tuple fact, so the tuple parent and tuple order aspects "work",
 * given the report (or this tuple) as starting point.
 */
final case class TupleFact(
  idOption: Option[String],
  aspectValues: Set[AspectValue],
  childFacts: immutable.IndexedSeq[Fact]) extends Fact {

  def descendantFacts: immutable.IndexedSeq[Fact] = {
    childFacts.flatMap {
      case simpleFact: SimpleFact =>
        immutable.IndexedSeq(simpleFact)
      case tupleFact: TupleFact =>
        // Recursive call
        tupleFact +: tupleFact.descendantFacts
    }
  }

  /**
   * Finds the (optional) fact within the parent tuple at the given relative path (this tuple itself if
   * the path is empty), and the given zero-based fact order within that parent tuple.
   */
  def findDescendantFact(relativePath: Path, orderInParentTuple: Int): Option[Fact] = {
    if (relativePath.isEmpty) {
      val childFactCount = childFacts.size

      if (orderInParentTuple >= 0 && orderInParentTuple < childFactCount) {
        Some(childFacts(orderInParentTuple))
      } else {
        None
      }
    } else {
      val firstPathEntry = relativePath.firstEntry
      val childFactName = firstPathEntry.elementName
      val childFactOption =
        childFacts.filter(_.conceptName == childFactName).drop(firstPathEntry.index).headOption
      val childTupleOption = childFactOption.collect { case f: TupleFact => f }

      // Recursive call

      childTupleOption.flatMap(_.findDescendantFact(relativePath.withoutFirstEntry, orderInParentTuple))
    }
  }

  def getDescendantFact(relativePath: Path, orderInParentTuple: Int): Fact = {
    findDescendantFact(relativePath, orderInParentTuple).getOrElse {
      sys.error(s"No fact found at path $relativePath and 0-based order in parent $orderInParentTuple")
    }
  }
}
