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

package eu.cdevreeze.xbrl.formula.varset

import scala.collection.immutable

import eu.cdevreeze.xbrl.formula.oim.Fact
import eu.cdevreeze.xbrl.formula.oim.SimpleFact
import eu.cdevreeze.xbrl.formula.oim.SimpleValue
import eu.cdevreeze.xbrl.formula.oim.StringValue

/**
 * Fact or fallback value holder.
 *
 * @author Chris de Vreeze
 */
sealed trait FactOrFallbackValueHolder {

  def value: Any

  def asFactOption: Option[Fact]
}

final case class FactHolder(fact: Fact) extends FactOrFallbackValueHolder {

  def value: SimpleValue = fact match {
    case f: SimpleFact => f.factValueOption.getOrElse(StringValue(""))
    case _ => StringValue("")
  }

  def asFactOption: Option[Fact] = Some(fact)
}

sealed trait FallbackValueHolder extends FactOrFallbackValueHolder {

  final def asFactOption: Option[Fact] = None
}

final case class NonSeqFallbackValueHolder(v: SimpleValue) extends FallbackValueHolder {

  def value: SimpleValue = v
}

final case class SeqFallbackValueHolder(v: immutable.IndexedSeq[SimpleValue]) extends FallbackValueHolder {

  def value: immutable.IndexedSeq[SimpleValue] = v
}
