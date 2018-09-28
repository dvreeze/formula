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

/**
 * Fact, so either a simple fact or a tuple fact.
 *
 * @author Chris de Vreeze
 */
sealed trait Fact {

  def id: String

  /**
   * The aspect values of this fact. No aspect values in this set may have the same aspect. The aspects
   * must be applicable to the kind of fact.
   */
  def aspectValues: Set[AspectValue]

  // TODO Footnotes
}

sealed trait SimpleFact extends Fact {

  def factValue: SimpleFactValue
}

final case class NonNumericSimpleFact(factValue: SimpleFactValue) extends SimpleFact

final case class NumericSimpleFact(
  factValue: SimpleFactValue,
  accuracy: Accuracy) extends SimpleFact

/**
 * Tuple fact. The child facts are contained in this tuple fact, so the tuple parent aspects "work",
 * given the report as starting point.
 */
final case class TupleFact(childFacts: immutable.IndexedSeq[Fact]) extends Fact
