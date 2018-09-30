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

import java.net.URI

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path

/**
 * Aspect value set, designed as a "value class". This class makes sure that no duplicate aspects occur
 * (for internal consistency). It contains (functional) builders for adding aspect value sets, and it has methods
 * filling in missing aspect values where required.
 *
 * @author Chris de Vreeze
 */
final class AspectValueSet private (val aspectValues: Set[AspectValue]) {
  require(aspectValues.size == aspectValues.toSeq.map(_.aspect).distinct.size, s"Duplicate aspects not allowed")

  // Query methods

  def aspects: Set[Aspect] = {
    aspectValues.toSeq.map(_.aspect).toSet
  }

  def findAspectValue(aspect: Aspect): Option[AspectValue] = {
    aspectValues.find(_.aspect == aspect)
  }

  def getAspectValue(aspect: Aspect): AspectValue = {
    findAspectValue(aspect).getOrElse(sys.error(s"Missing aspect value for aspect $aspect"))
  }

  // Equality and hashCode

  override def equals(other: Any): Boolean = {
    other match {
      case other: AspectValueSet => this.aspectValues == other.aspectValues
      case _ => false
    }
  }

  override def hashCode: Int = {
    aspectValues.hashCode
  }

  // Query methods specific for the different aspects

  def findConceptAspectValue: Option[ConceptAspectValue] = {
    findAspectValue(ConceptAspect).collect { case av: ConceptAspectValue => av }
  }

  def findEntityAspectValue: Option[EntityAspectValue] = {
    findAspectValue(EntityAspect).collect { case av: EntityAspectValue => av }
  }

  def findPeriodAspectValue: Option[PeriodAspectValue] = {
    findAspectValue(PeriodAspect).collect { case av: PeriodAspectValue => av }
  }

  def findTupleParentAspectValue: Option[TupleParentAspectValue] = {
    findAspectValue(TupleParentAspect).collect { case av: TupleParentAspectValue => av }
  }

  def findTupleOrderAspectValue: Option[TupleOrderAspectValue] = {
    findAspectValue(TupleOrderAspect).collect { case av: TupleOrderAspectValue => av }
  }

  def findLanguageAspectValue: Option[LanguageAspectValue] = {
    findAspectValue(LanguageAspect).collect { case av: LanguageAspectValue => av }
  }

  def findUnitAspectValue: Option[UnitAspectValue] = {
    findAspectValue(UnitAspect).collect { case av: UnitAspectValue => av }
  }

  def findAllDimensionAspectValues: Set[DimensionAspectValue] = {
    aspectValues.collect { case av: DimensionAspectValue => av }
  }

  def findAllExplicitDimensionAspectValues: Set[ExplicitDimensionAspectValue] = {
    aspectValues.collect { case av: ExplicitDimensionAspectValue => av }
  }

  def findAllTypedDimensionAspectValues: Set[TypedDimensionAspectValue] = {
    aspectValues.collect { case av: TypedDimensionAspectValue => av }
  }

  // Generally available functional updates

  def filteringAspects(aspects: Set[Aspect]): AspectValueSet = {
    new AspectValueSet(this.aspectValues.filter(av => aspects.contains(av.aspect)))
  }

  def withoutAspects(aspects: Set[Aspect]): AspectValueSet = {
    filteringAspects(this.aspects.diff(aspects))
  }

  /**
   * Adds the aspect value if its aspect does not yet occur, and replaces it if its aspect already occurs.
   * In other words, the added aspect value takes precedence.
   */
  def addOrUpdate(aspectValue: AspectValue): AspectValueSet = {
    new AspectValueSet(
      this.aspectValues.filterNot(_.aspect == aspectValue.aspect) + aspectValue)
  }

  /**
   * Adds the aspect value if its aspect does not yet occur, and returns this object unaltered otherwise.
   */
  def addIfAbsent(aspectValue: AspectValue): AspectValueSet = {
    if (findAspectValue(aspectValue.aspect).isEmpty) {
      new AspectValueSet(this.aspectValues + aspectValue)
    } else {
      this
    }
  }

  /**
   * Repeatedly invokes overloaded method addOrUpdate for a single aspect value, for each aspect value in newAspectValues.
   */
  def addOrUpdate(newAspectValues: Set[AspectValue]): AspectValueSet = {
    newAspectValues.foldLeft(this) {
      case (acc, nextAspectValue) =>
        acc.addOrUpdate(nextAspectValue)
    }
  }

  /**
   * Repeatedly invokes overloaded method addIfAbsent for a single aspect value, for each aspect value in newAspectValues.
   */
  def addIfAbsent(newAspectValues: Set[AspectValue]): AspectValueSet = {
    newAspectValues.foldLeft(this) {
      case (acc, nextAspectValue) =>
        acc.addIfAbsent(nextAspectValue)
    }
  }

  // Functional updates specific for the different aspects

  def withConcept(aspectValue: ConceptAspectValue): AspectValueSet = {
    addOrUpdate(aspectValue)
  }

  def withConcept(name: EName): AspectValueSet = {
    withConcept(ConceptAspectValue(name))
  }

  def withEntity(aspectValue: EntityAspectValue): AspectValueSet = {
    addOrUpdate(aspectValue)
  }

  def withEntity(scheme: URI, identifier: String): AspectValueSet = {
    withEntity(EntityAspectValue(scheme, identifier))
  }

  def withPeriod(aspectValue: PeriodAspectValue): AspectValueSet = {
    addOrUpdate(aspectValue)
  }

  def withPeriod(periodValue: PeriodValue): AspectValueSet = {
    withPeriod(PeriodAspectValue(periodValue))
  }

  def withTupleParent(tupleParentAspectValue: TupleParentAspectValue, tupleOrderAspectValue: TupleOrderAspectValue): AspectValueSet = {
    addOrUpdate(Set[AspectValue](tupleParentAspectValue, tupleOrderAspectValue))
  }

  def withTupleParent(parentPath: Path, zeroBasedOrderOption: Option[Int]): AspectValueSet = {
    withTupleParent(TupleParentAspectValue(parentPath), TupleOrderAspectValue(zeroBasedOrderOption))
  }

  def makeTopLevel: AspectValueSet = {
    withTupleParent(TupleParentAspectValue.Empty, TupleOrderAspectValue.Empty)
  }

  def withLanguage(aspectValue: LanguageAspectValue): AspectValueSet = {
    addOrUpdate(aspectValue)
  }

  def withLanguage(languageOption: Option[String]): AspectValueSet = {
    withLanguage(LanguageAspectValue(languageOption))
  }

  def withDimensions(dimensionAspectValues: Set[DimensionAspectValue]): AspectValueSet = {
    addOrUpdate(dimensionAspectValues.map(_.asInstanceOf[AspectValue]))
  }

  def withDimension(aspectValue: DimensionAspectValue): AspectValueSet = {
    addOrUpdate(aspectValue)
  }

  def withExplicitDimension(dim: EName, member: EName): AspectValueSet = {
    withDimension(ExplicitDimensionAspectValue(dim, member))
  }

  def withTypedDimension(dim: EName, member: TypedDimensionMember): AspectValueSet = {
    withDimension(TypedDimensionAspectValue(dim, member))
  }

  def withUnit(aspectValue: UnitAspectValue): AspectValueSet = {
    addOrUpdate(aspectValue)
  }

  // Specific functional updates for different kinds of facts

  def addMissingDefaultsForNonNumericSimpleFacts: AspectValueSet = {
    addIfAbsent(
      Set[AspectValue](TupleParentAspectValue.Empty, TupleOrderAspectValue.Empty, LanguageAspectValue.Empty))
  }

  def addMissingDefaultsForNumericSimpleFacts: AspectValueSet = {
    addIfAbsent(
      Set[AspectValue](TupleParentAspectValue.Empty, TupleOrderAspectValue.Empty))
  }

  def addMissingDefaultsForTupleFacts: AspectValueSet = {
    addIfAbsent(
      Set[AspectValue](TupleParentAspectValue.Empty, TupleOrderAspectValue.Empty))
  }

  // Validation for different kinds of facts

  /**
   * Validates if all required aspects for non-numeric simple facts are present, returning this object
   * if true and throwing an exception otherwise. The tuple parent/order aspects and language aspect are also required!
   */
  def validatedForNonNumericSimpleFacts: AspectValueSet = {
    val requiredAspects =
      Set[Aspect](ConceptAspect, EntityAspect, PeriodAspect, TupleParentAspect, TupleOrderAspect, LanguageAspect)

    require(
      requiredAspects.subsetOf(this.aspects),
      s"Not all required aspects are present")
    this
  }

  /**
   * Validates if all required aspects for numeric simple facts are present, returning this object
   * if true and throwing an exception otherwise. The tuple parent/order aspects are also required!
   */
  def validatedForNumericSimpleFacts: AspectValueSet = {
    val requiredAspects =
      Set[Aspect](ConceptAspect, EntityAspect, PeriodAspect, TupleParentAspect, TupleOrderAspect, UnitAspect)

    require(
      requiredAspects.subsetOf(this.aspects),
      s"Not all required aspects are present")
    this
  }

  /**
   * Validates if all required aspects for tuple facts are present, returning this object
   * if true and throwing an exception otherwise. The tuple parent/order aspects are also required!
   */
  def validatedForTupleFacts: AspectValueSet = {
    val requiredAspects =
      Set[Aspect](ConceptAspect, TupleParentAspect, TupleOrderAspect)

    require(
      requiredAspects.subsetOf(this.aspects),
      s"Not all required aspects are present")
    this
  }
}

object AspectValueSet {

  val Empty = new AspectValueSet(Set.empty)

  /**
   * Creates an AspectValueSet from the given aspect values. If there are duplicate aspects,
   * it is undefined which aspect value for that aspect wins.
   */
  def from(aspectValues: Set[AspectValue]): AspectValueSet = {
    aspectValues.foldLeft(Empty) {
      case (acc, aspectValue) =>
        acc.addOrUpdate(aspectValue)
    }
  }
}