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
 * Aspect value, so either a core aspect value or a taxonomy-defined aspect value.
 * This corresponds to an aspect in the OIM, whereas an Aspect corresponds to an aspect name in the OIM.
 *
 * Aspect values are very easy and cheap to compare for equality, partly because that is also the case
 * for aspects (without the values).
 *
 * @author Chris de Vreeze
 */
sealed trait AspectValue {

  type AspectType <: Aspect

  def aspect: AspectType
}

sealed trait CoreAspectValue extends AspectValue {

  type AspectType <: CoreAspect
}

sealed trait TaxonomyDefinedAspectValue extends AspectValue {

  type AspectType <: TaxonomyDefinedAspect
}

final case class ConceptAspectValue(conceptName: EName) extends CoreAspectValue {

  type AspectType = ConceptAspect.type

  def aspect: AspectType = ConceptAspect
}

final case class EntityAspectValue(scheme: URI, identifier: String) extends CoreAspectValue {

  type AspectType = EntityAspect.type

  def aspect: AspectType = EntityAspect
}

final case class PeriodAspectValue(periodValue: PeriodValue) extends CoreAspectValue {

  type AspectType = PeriodAspect.type

  def aspect: AspectType = PeriodAspect

  def isInstant: Boolean = periodValue.isInstant

  def isDuration: Boolean = periodValue.isDuration
}

final case class UnitAspectValue(numerators: Set[EName], denominators: Set[EName]) extends CoreAspectValue {

  type AspectType = UnitAspect.type

  def aspect: AspectType = UnitAspect
}

/**
 * Tuple parent aspect value, which contains the relative path (to the xbrli:xbrl root) of the parent
 * of this fact. If the parent path is empty, this fact is not inside a tuple but is instead a top-level fact.
 */
final case class TupleParentAspectValue(parentPath: Path) extends CoreAspectValue {

  type AspectType = TupleParentAspect.type

  def aspect: AspectType = TupleParentAspect
}

/**
 * Tuple order aspect value, which contains the relative order within the parent tuple, if the parent is a
 * tuple.
 */
final case class TupleOrderAspectValue(zeroBasedOrderOption: Option[Int]) extends CoreAspectValue {

  type AspectType = TupleOrderAspect.type

  def aspect: AspectType = TupleOrderAspect

  def oneBasedOrderOption: Option[Int] = zeroBasedOrderOption.map(_ + 1)
}

/**
 * Language aspect value, which contains the optional language code (BCP47).
 */
final case class LanguageAspectValue(languageCodeOption: Option[String]) extends CoreAspectValue {

  type AspectType = LanguageAspect.type

  def aspect: AspectType = LanguageAspect
}

sealed trait DimensionAspectValue extends TaxonomyDefinedAspectValue {

  type AspectType <: DimensionAspect

  final def dimension: EName = aspect.dimension
}

final case class ExplicitDimensionAspectValue(
  aspect: ExplicitDimensionAspect,
  member: EName) extends DimensionAspectValue {

  type AspectType = ExplicitDimensionAspect
}

final case class TypedDimensionAspectValue(
  aspect: TypedDimensionAspect,
  member: SimpleValue) extends DimensionAspectValue {

  type AspectType = TypedDimensionAspect
}

object UnitAspectValue {

  def fromNumerators(numerators: Set[EName]): UnitAspectValue = {
    UnitAspectValue(numerators, Set.empty)
  }
}

object TupleParentAspectValue {

  val Empty = TupleParentAspectValue(Path.Empty)
}

object TupleOrderAspectValue {

  val Empty = TupleOrderAspectValue(None)
}

object LanguageAspectValue {

  val Empty = LanguageAspectValue(None)
}

object ExplicitDimensionAspectValue {

  def apply(dimension: EName, member: EName): ExplicitDimensionAspectValue = {
    ExplicitDimensionAspectValue(ExplicitDimensionAspect(dimension), member)
  }
}

object TypedDimensionAspectValue {

  def apply(dimension: EName, member: SimpleValue): TypedDimensionAspectValue = {
    TypedDimensionAspectValue(TypedDimensionAspect(dimension), member)
  }
}
