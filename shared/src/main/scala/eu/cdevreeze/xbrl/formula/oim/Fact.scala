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
   * The aspect values of this fact. The aspects must be applicable to the kind of fact.
   */
  def aspectValueSet: AspectValueSet

  final def conceptName: EName = {
    conceptAspectValue.conceptName
  }

  final def conceptAspectValue: ConceptAspectValue = {
    aspectValueSet.findConceptAspectValue
      .getOrElse(sys.error(s"Missing concept aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def tupleParentAspectValue: TupleParentAspectValue = {
    aspectValueSet.findTupleParentAspectValue
      .getOrElse(sys.error(s"Missing tuple parent aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def tupleOrderAspectValue: TupleOrderAspectValue = {
    aspectValueSet.findTupleOrderAspectValue
      .getOrElse(sys.error(s"Missing tuple order aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  // TODO Footnotes
}

sealed trait SimpleFact extends Fact {

  /**
   * Returns the optional fact value, which is empty if nil.
   */
  def factValueOption: Option[SimpleValue]

  final def isNil: Boolean = {
    factValueOption.isEmpty
  }

  final def entityAspectValue: EntityAspectValue = {
    aspectValueSet.findEntityAspectValue
      .getOrElse(sys.error(s"Missing entity aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def periodAspectValue: PeriodAspectValue = {
    aspectValueSet.findPeriodAspectValue
      .getOrElse(sys.error(s"Missing period aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def languageAspectValue: LanguageAspectValue = {
    aspectValueSet.findLanguageAspectValue
      .getOrElse(sys.error(s"Missing language aspect in fact with ID ${idOption.getOrElse("")}"))
  }

  final def dimensionAspectValues: Set[DimensionAspectValue] = {
    aspectValueSet.findAllDimensionAspectValues
  }

  final def explicitDimensionAspectValues: Set[ExplicitDimensionAspectValue] = {
    aspectValueSet.findAllExplicitDimensionAspectValues
  }

  final def typedDimensionAspectValues: Set[TypedDimensionAspectValue] = {
    aspectValueSet.findAllTypedDimensionAspectValues
  }

  final def findDimensionAspectValue(dimension: EName): Option[DimensionAspectValue] = {
    aspectValueSet.findDimensionAspectValue(dimension)
  }

  final def findExplicitDimensionAspectValue(dimension: EName): Option[ExplicitDimensionAspectValue] = {
    aspectValueSet.findExplicitDimensionAspectValue(dimension)
  }

  final def findTypedDimensionAspectValue(dimension: EName): Option[TypedDimensionAspectValue] = {
    aspectValueSet.findTypedDimensionAspectValue(dimension)
  }

  final def getDimensionAspectValue(dimension: EName): DimensionAspectValue = {
    findDimensionAspectValue(dimension).getOrElse {
      sys.error(s"No dimension aspect value found for dimension $dimension")
    }
  }

  final def getExplicitDimensionAspectValue(dimension: EName): ExplicitDimensionAspectValue = {
    findExplicitDimensionAspectValue(dimension).getOrElse {
      sys.error(s"No explicit dimension aspect value found for dimension $dimension")
    }
  }

  final def getTypedDimensionAspectValue(dimension: EName): TypedDimensionAspectValue = {
    findTypedDimensionAspectValue(dimension).getOrElse {
      sys.error(s"No typed dimension aspect value found for dimension $dimension")
    }
  }
}

final case class NonNumericSimpleFact(
  idOption: Option[String],
  aspectValueSet: AspectValueSet,
  factValueOption: Option[SimpleValue]) extends SimpleFact

final case class NumericSimpleFact(
  idOption: Option[String],
  aspectValueSet: AspectValueSet,
  factValueOption: Option[SimpleValue],
  accuracy: Accuracy) extends SimpleFact {

  def unitAspectValue: UnitAspectValue = {
    aspectValueSet.findUnitAspectValue
      .getOrElse(sys.error(s"Missing unit aspect in fact with ID ${idOption.getOrElse("")}"))
  }
}

/**
 * Tuple fact. The child facts are contained in this tuple fact, so the tuple parent and tuple order aspects "work",
 * given the report (or this tuple) as starting point.
 */
final case class TupleFact(
  idOption: Option[String],
  aspectValueSet: AspectValueSet,
  childFacts: immutable.IndexedSeq[Fact]) extends Fact {

  def findAllDescendantFacts: immutable.IndexedSeq[Fact] = {
    childFacts.flatMap {
      case simpleFact: SimpleFact =>
        immutable.IndexedSeq(simpleFact)
      case tupleFact: TupleFact =>
        // Recursive call
        tupleFact +: tupleFact.findAllDescendantFacts
    }
  }

  def findAllDescendantSimpleFacts: immutable.IndexedSeq[SimpleFact] = {
    findAllDescendantFacts.collect { case f: SimpleFact => f }
  }

  def findAllDescendantNumericSimpleFacts: immutable.IndexedSeq[NumericSimpleFact] = {
    findAllDescendantFacts.collect { case f: NumericSimpleFact => f }
  }

  def findAllDescendantTupleFacts: immutable.IndexedSeq[TupleFact] = {
    findAllDescendantFacts.collect { case f: TupleFact => f }
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

object NonNumericSimpleFact {

  /**
   * Factory method like apply, but adding default aspect values and validating the aspect values.
   */
  def from(
    idOption: Option[String],
    aspectValueSet: AspectValueSet,
    factValueOption: Option[SimpleValue]): NonNumericSimpleFact = {

    NonNumericSimpleFact(
      idOption,
      aspectValueSet
        .addDefaultsForNonNumericSimpleFacts
        .validatedForNonNumericSimpleFacts,
      factValueOption)
  }
}

object NumericSimpleFact {

  /**
   * Factory method like apply, but adding default aspect values and validating the aspect values.
   */
  def from(
    idOption: Option[String],
    aspectValueSet: AspectValueSet,
    factValueOption: Option[SimpleValue],
    accuracy: Accuracy): NumericSimpleFact = {

    NumericSimpleFact(
      idOption,
      aspectValueSet
        .addDefaultsForNumericSimpleFacts
        .validatedForNumericSimpleFacts,
      factValueOption,
      accuracy)
  }
}

object TupleFact {

  /**
   * Factory method like apply, but adding default aspect values and validating the aspect values.
   */
  def from(
    idOption: Option[String],
    aspectValueSet: AspectValueSet,
    childFacts: immutable.IndexedSeq[Fact]): TupleFact = {

    TupleFact(
      idOption,
      aspectValueSet
        .addDefaultsForTupleFacts
        .validatedForTupleFacts,
      childFacts)
  }
}
