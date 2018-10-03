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
import java.time.temporal.Temporal

/**
 * Period value, so either Forever or a time interval (with or without timezone).
 * These period values are by design easy to compare for value equality. Indeed, they may depend on
 * JDK LocalDateTime and ZonedDateTime objects, which themselves are considered "value classes".
 *
 * @author Chris de Vreeze
 */
sealed trait PeriodValue {

  def isInstant: Boolean

  final def isDuration: Boolean = !isInstant

  final def isFiniteDuration: Boolean = isDuration && !isForever

  final def isForever: Boolean = this == Forever

  final def isTimeInterval: Boolean = !isForever

  def asTimeInterval: TimeInterval

  final def asOptionalTimeInterval: Option[TimeInterval] = {
    if (isTimeInterval) Some(asTimeInterval) else None
  }
}

case object Forever extends PeriodValue {

  def isInstant: Boolean = false

  def asTimeInterval: TimeInterval = sys.error("Not a time interval but 'forever'")
}

sealed trait TimeInterval extends PeriodValue {

  def start: Temporal

  def end: Temporal

  final def asTimeInterval: TimeInterval = this

  def atStart: TimeInterval

  def atEnd: TimeInterval
}

final case class LocalTimeInterval(start: LocalDateTime, end: LocalDateTime) extends TimeInterval {

  def isInstant: Boolean = (start == end)

  def atStart: LocalTimeInterval = LocalTimeInterval(start, start)

  def atEnd: LocalTimeInterval = LocalTimeInterval(end, end)
}

final case class ZonedTimeInterval(start: ZonedDateTime, end: ZonedDateTime) extends TimeInterval {

  def isInstant: Boolean = (start == end)

  def atStart: ZonedTimeInterval = ZonedTimeInterval(start, start)

  def atEnd: ZonedTimeInterval = ZonedTimeInterval(end, end)
}

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
