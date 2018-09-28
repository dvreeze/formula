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

  def aspectConstraint: AspectConstraint
}

sealed trait CoreAspect extends Aspect

sealed trait TaxonomyDefinedAspect extends Aspect {

  final def aspectConstraint: AspectConstraint = AspectConstraint.AllSimpleFacts
}

case object ConceptAspect extends CoreAspect {

  def aspectConstraint: AspectConstraint = AspectConstraint.AllFacts
}

case object EntityAspect extends CoreAspect {

  def aspectConstraint: AspectConstraint = AspectConstraint.AllSimpleFacts
}

case object PeriodAspect extends CoreAspect {

  def aspectConstraint: AspectConstraint = AspectConstraint.AllSimpleFacts
}

case object UnitAspect extends CoreAspect {

  def aspectConstraint: AspectConstraint = AspectConstraint.AllNumericSimpleFacts
}

case object TupleParentAspect extends CoreAspect {

  def aspectConstraint: AspectConstraint = AspectConstraint.AllFacts
}

case object TupleOrderAspect extends CoreAspect {

  def aspectConstraint: AspectConstraint = AspectConstraint.AllFacts
}

case object LanguageAspect extends CoreAspect {

  def aspectConstraint: AspectConstraint = AspectConstraint.AllSimpleFacts
}

sealed trait DimensionAspect extends TaxonomyDefinedAspect {
  def dimension: EName
}

final case class ExplicitDimensionAspect(dimension: EName) extends DimensionAspect

final case class TypedDimensionAspect(dimension: EName) extends DimensionAspect
