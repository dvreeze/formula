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
import java.time.LocalTime
import java.time.ZonedDateTime

import scala.util.Try

import eu.cdevreeze.tqa.ENames
import eu.cdevreeze.tqa.XsdBooleans
import eu.cdevreeze.yaidom.core.EName

/**
 * Simple value, that is, a value of a simple schema type. See https://www.w3.org/TR/xmlschema-2/#built-in-datatypes.
 * Also see http://www.datypic.com/sc/xsd/s-datatypes.xsd.html. Also see JAXB and the DatatypeConverter class.
 *
 * All these SimpleValue sub-types are "value classes", with a well-defined equality operation.
 *
 * The type hierarchy is also designed in such a way that each value naturally falls in only one of these types.
 *
 * There is currently no support for built-in primitive types such as xs:duration, xs:time with timezone,
 * xs:date with timezone, xs:gYearMonth, xs:gYear, xs:gMonthDay, xs:gDay, xs:gMonth, xs:base64Binary (treated as
 * string instead, but mind encoding), xs:hexBinary (treated as string instead, but mind encoding), xs:float,
 * xs:double, xs:anyURI (treated as string instead), xs:QName, xs:NOTATION.
 *
 * @author Chris de Vreeze
 */
sealed trait SimpleValue {

  def value: Any
}

/**
 * A value corresponding to type xs:string, or one of its sub-types. If the value is of type xs:normalizedString
 * or a sub-type, the normalized value should be stored in this object.
 */
final case class StringValue(value: String) extends SimpleValue

/**
 * A value corresponding to type xs:decimal, or one of its sub-types.
 */
final case class NumericValue(value: BigDecimal) extends SimpleValue

/**
 * A value corresponding to type xs:boolean.
 */
final case class BooleanValue(value: Boolean) extends SimpleValue

/**
 * A value corresponding to type xs:dateTime, without a timezone.
 */
final case class LocalDateTimeValue(value: LocalDateTime) extends SimpleValue

/**
 * A value corresponding to type xs:dateTime, with a timezone.
 */
final case class ZonedDateTimeValue(value: ZonedDateTime) extends SimpleValue

/**
 * A value corresponding to type xs:date, without a timezone.
 */
final case class LocalDateValue(value: LocalDate) extends SimpleValue

/**
 * A value corresponding to type xs:time, without a timezone.
 */
final case class LocalTimeValue(value: LocalTime) extends SimpleValue

object SimpleValue {

  import ENames._

  /**
   * Parses the given value into a SimpleValue, using the given schema type. The schema type
   * must be built-in, or else the value is treated as a StringValue.
   */
  def parse(v: String, schemaType: EName): SimpleValue = schemaType match {
    case XsBooleanEName =>
      BooleanValue(XsdBooleans.parseBoolean(v))
    case XsStringEName =>
      StringValue(v)
    case tpe if isBuiltInStringType(tpe) =>
      StringValue(normalizeString(v))
    case tpe if isBuiltInDecimalType(tpe) =>
      // TODO Does this work in all cases? See http://www.datypic.com/sc/xsd/t-xsd_decimal.html.
      NumericValue(BigDecimal(v))
    case XsDateTimeEName =>
      Try(LocalDateTimeValue(LocalDateTime.parse(v)))
        .orElse(Try(ZonedDateTimeValue(ZonedDateTime.parse(v))))
        .getOrElse(StringValue(v))
    case XsDateEName =>
      Try(LocalDateValue(LocalDate.parse(v)))
        .getOrElse(StringValue(v))
    case XsTimeEName =>
      Try(LocalTimeValue(LocalTime.parse(v)))
        .getOrElse(StringValue(v))
    case _ =>
      StringValue(v)
  }

  private def isBuiltInDecimalType(schemaType: EName): Boolean = {
    schemaType == XsDecimalEName ||
      schemaType == XsIntegerEName ||
      schemaType == XsNonPositiveIntegerEName ||
      schemaType == XsNonNegativeIntegerEName ||
      schemaType == XsLongEName ||
      schemaType == XsNegativeIntegerEName ||
      schemaType == XsIntEName ||
      schemaType == XsUnsignedLongEName ||
      schemaType == XsPositiveIntegerEName ||
      schemaType == XsShortEName ||
      schemaType == XsUnsignedIntEName ||
      schemaType == XsByteEName ||
      schemaType == XsUnsignedShortEName ||
      schemaType == XsUnsignedByteEName
  }

  private def isBuiltInStringType(schemaType: EName): Boolean = {
    schemaType == XsStringEName ||
      schemaType == XsNormalizedStringEName ||
      schemaType == XsTokenEName ||
      schemaType == XsLanguageEName ||
      schemaType == XsNameEName ||
      schemaType == XsNMTOKEN_EName ||
      schemaType == XsNCNameEName ||
      schemaType == XsNMTOKENS_EName ||
      schemaType == XsID_EName ||
      schemaType == XsIDREF_EName ||
      schemaType == XsENTITY_EName ||
      schemaType == XsIDREFS_EName ||
      schemaType == XsENTITIES_EName
  }

  /**
   * Normalizes the string, removing surrounding whitespace and normalizing internal whitespace to a single space.
   * Whitespace includes #x20 (space), #x9 (tab), #xD (carriage return), #xA (line feed). If there is only whitespace,
   * the empty string is returned. Inspired by the JDOM library.
   */
  private def normalizeString(s: String): String = {
    require(s ne null) // scalastyle:off null

    val separators = Array(' ', '\t', '\r', '\n')
    val words: Seq[String] = s.split(separators).toSeq.filterNot(_.isEmpty)

    words.mkString(" ") // Returns empty string if words.isEmpty
  }
}
