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

import eu.cdevreeze.yaidom.core.EName

/**
 * Aspect, so either a core aspect or a taxonomy-defined aspect.
 * This corresponds to an aspect name in the OIM, whereas an AspectValue corresponds to an aspect in the OIM.
 *
 * Aspects are very easy to compare for equality, which is important, because this must be the case for
 * aspect values as well.
 *
 * @author Chris de Vreeze
 */
sealed trait Aspect {

  def aspectName: EName

  def aspectConstraint: AspectConstraint
}

sealed trait CoreAspect extends Aspect

sealed trait TaxonomyDefinedAspect extends Aspect {

  final def aspectConstraint: AspectConstraint = AspectConstraint.SimpleFacts
}

case object ConceptAspect extends CoreAspect {

  def aspectName: EName = Aspect.OimConceptEName

  def aspectConstraint: AspectConstraint = AspectConstraint.SimpleAndTupleFacts
}

case object EntityAspect extends CoreAspect {

  def aspectName: EName = Aspect.OimEntityEName

  def aspectConstraint: AspectConstraint = AspectConstraint.SimpleFacts
}

case object PeriodAspect extends CoreAspect {

  def aspectName: EName = Aspect.OimPeriodEName

  def aspectConstraint: AspectConstraint = AspectConstraint.SimpleFacts
}

case object UnitAspect extends CoreAspect {

  def aspectName: EName = Aspect.OimUnitEName

  def aspectConstraint: AspectConstraint = AspectConstraint.NumericSimpleFacts
}

case object TupleParentAspect extends CoreAspect {

  def aspectName: EName = Aspect.OimTupleParentEName

  def aspectConstraint: AspectConstraint = AspectConstraint.SimpleAndTupleFacts
}

case object TupleOrderAspect extends CoreAspect {

  def aspectName: EName = Aspect.OimTupleOrderEName

  def aspectConstraint: AspectConstraint = AspectConstraint.SimpleAndTupleFacts
}

case object LanguageAspect extends CoreAspect {

  def aspectName: EName = Aspect.OimLanguageEName

  def aspectConstraint: AspectConstraint = AspectConstraint.SimpleFacts
}

sealed trait DimensionAspect extends TaxonomyDefinedAspect {

  final def aspectName: EName = dimension

  def dimension: EName
}

final case class ExplicitDimensionAspect(dimension: EName) extends DimensionAspect

final case class TypedDimensionAspect(dimension: EName) extends DimensionAspect

object Aspect {

  val OimNamespace = "http://www.xbrl.org/CR/2017-05-02/oim"

  val OimConceptEName = EName(OimNamespace, "concept")
  val OimEntityEName = EName(OimNamespace, "entity")
  val OimPeriodEName = EName(OimNamespace, "period")
  val OimUnitEName = EName(OimNamespace, "unit")
  val OimTupleParentEName = EName(OimNamespace, "tupleParent")
  val OimTupleOrderEName = EName(OimNamespace, "tupleOrder")
  val OimLanguageEName = EName(OimNamespace, "language")

  def parseAspectName(name: EName, typedDimensions: Set[EName]): Aspect = name match {
    case OimConceptEName => ConceptAspect
    case OimEntityEName => EntityAspect
    case OimPeriodEName => PeriodAspect
    case OimUnitEName => UnitAspect
    case OimTupleParentEName => TupleParentAspect
    case OimTupleOrderEName => TupleOrderAspect
    case OimLanguageEName => LanguageAspect
    case name if typedDimensions.contains(name) => TypedDimensionAspect(name)
    case name => ExplicitDimensionAspect(name)
  }
}
