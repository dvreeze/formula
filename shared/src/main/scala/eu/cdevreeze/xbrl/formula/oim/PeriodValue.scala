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

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZonedDateTime

/**
 * Period value, so either Forever or a time interval (with or without timezone).
 *
 * @author Chris de Vreeze
 */
sealed trait PeriodValue

case object Forever extends PeriodValue

sealed trait TimeInterval extends PeriodValue

final case class LocalTimeInterval(start: LocalDateTime, end: LocalDateTime) extends TimeInterval

final case class ZonedTimeInterval(start: ZonedDateTime, end: ZonedDateTime) extends TimeInterval

object LocalTimeInterval {

  def apply(instant: LocalDateTime): LocalTimeInterval = {
    LocalTimeInterval(instant, instant)
  }

  def fromLocalDate(instant: LocalDate): LocalTimeInterval = {
    apply(instant.atStartOfDay().plusDays(1))
  }

  def fromLocalDates(start: LocalDate, end: LocalDate): LocalTimeInterval = {
    LocalTimeInterval(start.atStartOfDay(), end.atStartOfDay().plusDays(1))
  }
}

object ZonedTimeInterval {

  def apply(instant: ZonedDateTime): ZonedTimeInterval = {
    ZonedTimeInterval(instant, instant)
  }
}
