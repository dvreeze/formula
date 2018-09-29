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

package eu.cdevreeze.xbrl.formula.xmlmapping

import java.net.URI
import java.time.LocalDateTime
import java.time.ZonedDateTime

import scala.math.floor
import scala.math.log10

import eu.cdevreeze.tqa.base.queryapi.TaxonomyApi
import eu.cdevreeze.tqa.ENames
import eu.cdevreeze.tqa.Namespaces
import eu.cdevreeze.tqa.XsdBooleans
import eu.cdevreeze.tqa.instance
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.queryapi.BackingNodes
import eu.cdevreeze.xbrl.formula.oim.Accuracy
import eu.cdevreeze.xbrl.formula.oim.AspectValue
import eu.cdevreeze.xbrl.formula.oim.ConceptAspectValue
import eu.cdevreeze.xbrl.formula.oim.DtsReference
import eu.cdevreeze.xbrl.formula.oim.EntityAspectValue
import eu.cdevreeze.xbrl.formula.oim.ExplicitDimensionAspectValue
import eu.cdevreeze.xbrl.formula.oim.Fact
import eu.cdevreeze.xbrl.formula.oim.FiniteAccuracy
import eu.cdevreeze.xbrl.formula.oim.Forever
import eu.cdevreeze.xbrl.formula.oim.Infinity
import eu.cdevreeze.xbrl.formula.oim.LanguageAspectValue
import eu.cdevreeze.xbrl.formula.oim.LocalTimeInterval
import eu.cdevreeze.xbrl.formula.oim.NonNumericSimpleFact
import eu.cdevreeze.xbrl.formula.oim.NumericSimpleFact
import eu.cdevreeze.xbrl.formula.oim.PeriodAspectValue
import eu.cdevreeze.xbrl.formula.oim.PeriodValue
import eu.cdevreeze.xbrl.formula.oim.Report
import eu.cdevreeze.xbrl.formula.oim.SimpleFact
import eu.cdevreeze.xbrl.formula.oim.SimpleFactValue
import eu.cdevreeze.xbrl.formula.oim.TupleFact
import eu.cdevreeze.xbrl.formula.oim.TupleOrderAspectValue
import eu.cdevreeze.xbrl.formula.oim.TupleParentAspectValue
import eu.cdevreeze.xbrl.formula.oim.TypedDimensionAspectValue
import eu.cdevreeze.xbrl.formula.oim.TypedDimensionMember
import eu.cdevreeze.xbrl.formula.oim.UnitAspectValue
import eu.cdevreeze.xbrl.formula.oim.ZonedTimeInterval

/**
 * Mapper from XBRL instances in XML format to the Open Information Model.
 *
 * The mapper takes a DTS as state, and each instance to be mapped must belong to that DTS.
 *
 * @author Chris de Vreeze
 */
final class XmlToOimMapper(dts: TaxonomyApi) {

  /**
   * Converts an XBRL instance in XML format to an OIM report. The entrypoint(s) in the instance must
   * occur in the DTS, or else an exception is thrown.
   */
  def convertXbrlInstance(xbrlInstance: instance.XbrlInstance): Report = {
    val schemaRefs = xbrlInstance.findAllSchemaRefs.map(e => convertSchemaRef(e))
    val linkbaseRefs = xbrlInstance.findAllLinkbaseRefs.map(e => convertLinkbaseRef(e))
    val roleRefs = xbrlInstance.findAllRoleRefs.map(e => convertRoleRef(e))
    val arcroleRefs = xbrlInstance.findAllArcroleRefs.map(e => convertArcroleRef(e))

    require(
      schemaRefs.map(_.href).toSet.subsetOf(dts.taxonomyDocs.map(_.uri).toSet),
      s"Not all schemaRefs found in the DTS")

    require(
      linkbaseRefs.map(_.href).toSet.subsetOf(dts.taxonomyDocs.map(_.uri).toSet),
      s"Not all linkbaseRefs found in the DTS")

    val dtsReferences = schemaRefs ++ linkbaseRefs ++ roleRefs ++ arcroleRefs

    val topLevelFacts = xbrlInstance.findAllTopLevelFacts.map(f => convertFact(f, xbrlInstance))

    new Report(dtsReferences, topLevelFacts)
  }

  def convertFact(fact: instance.Fact, xbrlInstance: instance.XbrlInstance): Fact = {
    fact match {
      case itemFact: instance.ItemFact => convertItemFact(itemFact, xbrlInstance)
      case tupleFact: instance.TupleFact => convertTupleFact(tupleFact, xbrlInstance)
    }
  }

  def convertItemFact(fact: instance.ItemFact, xbrlInstance: instance.XbrlInstance): SimpleFact = {
    fact match {
      case nonNumericItemFact: instance.NonNumericItemFact =>
        convertNonNumericItemFact(nonNumericItemFact, xbrlInstance)
      case numericItemFact: instance.NumericItemFact =>
        convertNumericItemFact(numericItemFact, xbrlInstance)
    }
  }

  def convertTupleFact(fact: instance.TupleFact, xbrlInstance: instance.XbrlInstance): TupleFact = {
    val idOption = fact.attributeOption(ENames.IdEName)

    val conceptAspectValue = extractConceptAspectValueFromFact(fact)
    val tupleParentAspectValue = extractTupleParentAspectValueFromFact(fact)
    val tupleOrderAspectValue = extractTupleOrderAspectValueFromFact(fact)

    val aspectValues = Set[AspectValue](conceptAspectValue, tupleParentAspectValue, tupleOrderAspectValue)

    // Recursive calls into convertFact

    val childFacts = fact.findAllChildFacts.map(f => convertFact(f, xbrlInstance))

    TupleFact(idOption, aspectValues, childFacts)
  }

  def convertNonNumericItemFact(fact: instance.NonNumericItemFact, xbrlInstance: instance.XbrlInstance): NonNumericSimpleFact = {
    val idOption = fact.attributeOption(ENames.IdEName)

    val context = xbrlInstance.getContextById(fact.contextRef)

    val aspectValuesFromContext = extractAspectValuesFromContext(context)

    val conceptAspectValue = extractConceptAspectValueFromFact(fact)
    val tupleParentAspectValue = extractTupleParentAspectValueFromFact(fact)
    val tupleOrderAspectValue = extractTupleOrderAspectValueFromFact(fact)
    val languageAspectValue = extractLanguageAspectValueFromItemFact(fact)

    val aspectValues =
      Set[AspectValue](
        conceptAspectValue,
        tupleParentAspectValue,
        tupleOrderAspectValue,
        languageAspectValue).union(aspectValuesFromContext)

    val factValue = extractNonNumericFactValue(fact)

    NonNumericSimpleFact(idOption, aspectValues, factValue)
  }

  def convertNumericItemFact(fact: instance.NumericItemFact, xbrlInstance: instance.XbrlInstance): NumericSimpleFact = {
    val idOption = fact.attributeOption(ENames.IdEName)

    val context = xbrlInstance.getContextById(fact.contextRef)
    val unit = xbrlInstance.getUnitById(fact.unitRef)

    val aspectValuesFromContext = extractAspectValuesFromContext(context)
    val unitAspectValue = extractUnitAspectValueFromUnit(unit)

    val conceptAspectValue = extractConceptAspectValueFromFact(fact)
    val tupleParentAspectValue = extractTupleParentAspectValueFromFact(fact)
    val tupleOrderAspectValue = extractTupleOrderAspectValueFromFact(fact)
    val languageAspectValue = LanguageAspectValue(None)

    val aspectValues =
      Set[AspectValue](
        conceptAspectValue,
        tupleParentAspectValue,
        tupleOrderAspectValue,
        languageAspectValue,
        unitAspectValue).union(aspectValuesFromContext)

    val factValue = extractNumericFactValue(fact)

    val accuracy = extractAccuracy(fact)

    NumericSimpleFact(idOption, aspectValues, factValue, accuracy)
  }

  def extractAspectValuesFromContext(context: instance.XbrliContext): Set[AspectValue] = {
    val entityAspectValue = extractEntityAspectValueFromContext(context)
    val periodAspectValue = extractPeriodAspectValueFromContext(context)

    val explicitDimensionAspectValues = extractExplicitDimensionAspectValuesFromContext(context)
    val typedDimensionAspectValues = extractTypedDimensionAspectValuesFromContext(context)

    Set[AspectValue](entityAspectValue, periodAspectValue)
      .union(explicitDimensionAspectValues.collect { case av: AspectValue => av })
      .union(typedDimensionAspectValues.collect { case av: AspectValue => av })
  }

  def extractUnitAspectValueFromUnit(unit: instance.XbrliUnit): UnitAspectValue = {
    val numerators = unit.numeratorMeasures
    val denominators = unit.denominatorMeasures

    UnitAspectValue(numerators.toSet, denominators.toSet)
  }

  def extractConceptAspectValueFromFact(fact: instance.Fact): ConceptAspectValue = {
    ConceptAspectValue(fact.resolvedName)
  }

  def extractEntityAspectValueFromContext(context: instance.XbrliContext): EntityAspectValue = {
    val scheme = URI.create(context.entity.identifierScheme)
    val identifier = context.entity.identifierValue

    EntityAspectValue(scheme, identifier)
  }

  def extractPeriodAspectValueFromContext(context: instance.XbrliContext): PeriodAspectValue = {
    val periodValue: PeriodValue =
      context.period match {
        case p: instance.ForeverPeriod =>
          Forever
        case p: instance.InstantPeriod if p.instantDateTime.isInstanceOf[LocalDateTime] =>
          val instantDateTime = p.instantDateTime.asInstanceOf[LocalDateTime]

          LocalTimeInterval(instantDateTime, instantDateTime)
        case p: instance.InstantPeriod =>
          require(
            p.instantDateTime.isInstanceOf[ZonedDateTime],
            s"Expected ZonedDateTime but got ${p.instantDateTime.getClass} for time ${p.instantDateTime}")

          val instantDateTime = p.instantDateTime.asInstanceOf[ZonedDateTime]

          ZonedTimeInterval(instantDateTime, instantDateTime)
        case p: instance.StartEndDatePeriod if p.startDateTime.isInstanceOf[LocalDateTime] =>
          require(
            p.endDateTime.isInstanceOf[LocalDateTime],
            s"Expected LocalDateTime but got ${p.endDateTime.getClass} for end time ${p.endDateTime}")

          val startDateTime = p.startDateTime.asInstanceOf[LocalDateTime]
          val endDateTime = p.endDateTime.asInstanceOf[LocalDateTime]

          LocalTimeInterval(startDateTime, endDateTime)
        case p: instance.StartEndDatePeriod =>
          require(
            p.startDateTime.isInstanceOf[ZonedDateTime],
            s"Expected ZonedDateTime but got ${p.startDateTime.getClass} for start time ${p.startDateTime}")
          require(
            p.endDateTime.isInstanceOf[ZonedDateTime],
            s"Expected ZonedDateTime but got ${p.endDateTime.getClass} for end time ${p.endDateTime}")

          val startDateTime = p.startDateTime.asInstanceOf[ZonedDateTime]
          val endDateTime = p.endDateTime.asInstanceOf[ZonedDateTime]

          ZonedTimeInterval(startDateTime, endDateTime)
      }

    PeriodAspectValue(periodValue)
  }

  // TODO How about default dimensions?

  def extractExplicitDimensionAspectValuesFromContext(context: instance.XbrliContext): Set[ExplicitDimensionAspectValue] = {
    context.explicitDimensionMembers.toSeq
      .map {
        case (dim, mem) =>
          ExplicitDimensionAspectValue(dim, mem)
      }
      .toSet
  }

  def extractTypedDimensionAspectValuesFromContext(context: instance.XbrliContext): Set[TypedDimensionAspectValue] = {
    context.typedDimensionMembers.toSeq
      .map {
        case (dim, mem) =>
          val convertedMember = extractTypedDimensionMember(dim, mem)

          TypedDimensionAspectValue(dim, convertedMember)
      }
      .toSet
  }

  def extractTupleParentAspectValueFromFact(fact: instance.Fact): TupleParentAspectValue = {
    TupleParentAspectValue(fact.path.parentPath)
  }

  def extractTupleOrderAspectValueFromFact(fact: instance.Fact): TupleOrderAspectValue = {
    // Somewhat expensive!

    val path = fact.path
    val lastPathEntry = path.lastEntry

    val parent = fact.backingElem.parent

    val parentFactOption: Option[BackingNodes.Elem] =
      if (parent.resolvedName.namespaceUriOption.contains(Namespaces.XbrliNamespace) && parent.localName == "xbrl") {
        None
      } else {
        Some(parent)
      }

    val zeroBasedOrderOption: Option[Int] =
      parentFactOption
        .map(_.findAllChildElemsWithPathEntries.zipWithIndex.find(_._1._2 == lastPathEntry).head._2)

    TupleOrderAspectValue(zeroBasedOrderOption)
  }

  def extractLanguageAspectValueFromItemFact(fact: instance.ItemFact): LanguageAspectValue = {
    // Somewhat expensive!

    val langOption = extractOptionalLanguage(fact.backingElem)

    LanguageAspectValue(langOption)
  }

  def convertSchemaRef(schemaRef: instance.SchemaRef): DtsReference.Schema = {
    DtsReference.Schema(schemaRef.resolvedHref)
  }

  def convertLinkbaseRef(schemaRef: instance.LinkbaseRef): DtsReference.Linkbase = {
    DtsReference.Linkbase(schemaRef.resolvedHref)
  }

  def convertRoleRef(schemaRef: instance.RoleRef): DtsReference.Role = {
    DtsReference.Role(schemaRef.resolvedHref)
  }

  def convertArcroleRef(schemaRef: instance.ArcroleRef): DtsReference.Arcrole = {
    DtsReference.Arcrole(schemaRef.resolvedHref)
  }

  // TODO Footnotes

  // Private methods

  private def extractTypedDimensionMember(dimension: EName, memberElem: instance.XbrliElem): TypedDimensionMember = {
    require(
      memberElem.findAllChildElems.isEmpty,
      s"Typed members with child elements not supported in OIM. Path: ${memberElem.backingElem.absolutePath}")

    val typedMemberDecl = dts.getMemberDeclarationOfTypedDimension(dimension)

    // TODO Note that if an anonymous type is used, it is ignored here! Fix this!

    val typedMemberTypeOption: Option[EName] = typedMemberDecl.typeOption

    val baseTypeOption: Option[EName] =
      typedMemberTypeOption.flatMap { tp =>
        dts.findBaseTypeOrSelfUntil(tp, _.namespaceUriOption.contains(Namespaces.XsNamespace))
      }

    val isBoolean = baseTypeOption.contains(ENames.XsBooleanEName)

    val isDecimal = baseTypeOption.map(tp => isDecimalType(tp)).getOrElse(false)

    val memberText = memberElem.text

    if (isBoolean) {
      TypedDimensionMember.BooleanValue(XsdBooleans.parseBoolean(memberText.trim))
    } else if (isDecimal) {
      TypedDimensionMember.NumericValue(BigDecimal(memberText.trim))
    } else {
      TypedDimensionMember.StringValue(memberText) // No trimming ever for strings?
    }
  }

  private def isDecimalType(tpe: EName): Boolean = {
    // See https://www.w3.org/TR/xmlschema-2/

    tpe == ENames.XsDecimalEName || tpe == ENames.XsIntegerEName ||
      tpe == ENames.XsNonPositiveIntegerEName || tpe == ENames.XsLongEName ||
      tpe == ENames.XsNonNegativeIntegerEName || tpe == ENames.XsIntEName ||
      tpe == ENames.XsShortEName || tpe == ENames.XsByteEName ||
      tpe == ENames.XsNegativeIntegerEName || tpe == ENames.XsPositiveIntegerEName ||
      tpe == ENames.XsUnsignedLongEName || tpe == ENames.XsUnsignedIntEName ||
      tpe == ENames.XsUnsignedShortEName || tpe == ENames.XsUnsignedByteEName
  }

  private def extractOptionalLanguage(elem: BackingNodes.Elem): Option[String] = {
    val xmlLangOption = elem.attributeOption(ENames.XmlLangEName).map(_.trim)

    xmlLangOption.orElse {
      val parentOption = elem.parentOption

      // Recursive call
      parentOption.flatMap(pe => extractOptionalLanguage(pe))
    }
  }

  private def extractNonNumericFactValue(itemFact: instance.NonNumericItemFact): SimpleFactValue = {
    if (itemFact.isNil) {
      SimpleFactValue.Nil
    } else {
      val itemDecl = dts.getItemDeclaration(itemFact.resolvedName)

      // TODO Note that if an anonymous type is used, it is ignored here! Fix this!

      val typeOption: Option[EName] = itemDecl.globalElementDeclaration.typeOption

      val baseTypeOption: Option[EName] =
        typeOption.flatMap { tp =>
          dts.findBaseTypeOrSelfUntil(tp, _.namespaceUriOption.contains(Namespaces.XsNamespace))
        }

      val isBoolean = baseTypeOption.contains(ENames.XsBooleanEName)

      val rawFactValue = itemFact.text

      if (isBoolean) {
        SimpleFactValue.BooleanValue(XsdBooleans.parseBoolean(rawFactValue.trim))
      } else {
        SimpleFactValue.StringValue(rawFactValue) // No trimming ever for strings?
      }
    }
  }

  private def extractNumericFactValue(itemFact: instance.NumericItemFact): SimpleFactValue = {
    if (itemFact.isNil) {
      SimpleFactValue.Nil
    } else {
      val itemDecl = dts.getItemDeclaration(itemFact.resolvedName)

      // TODO Note that if an anonymous type is used, it is ignored here! Fix this!

      val typeOption: Option[EName] = itemDecl.globalElementDeclaration.typeOption

      val baseTypeOption: Option[EName] =
        typeOption.flatMap { tp =>
          dts.findBaseTypeOrSelfUntil(tp, _.namespaceUriOption.contains(Namespaces.XsNamespace))
        }

      val isDecimal = baseTypeOption.map(tp => isDecimalType(tp)).getOrElse(false)

      val rawFactValue = itemFact.text

      if (isDecimal) {
        SimpleFactValue.NumericValue(BigDecimal(rawFactValue.trim))
      } else {
        SimpleFactValue.StringValue(rawFactValue) // No trimming ever for strings?
      }
    }
  }

  /**
   * Extracts the accuracy from the given numeric item fact. See section 4.6.6 of the XBRL specification.
   */
  private def extractAccuracy(numericItemFact: instance.NumericItemFact): Accuracy = {
    numericItemFact match {
      case f: instance.NilNumericItemFact =>
        Infinity
      case f: instance.NonNilFractionItemFact =>
        Infinity
      case f: instance.NonNilNonFractionNumericItemFact =>
        val decimalsOption = f.decimalsOption
        val precisionOption = f.precisionOption

        decimalsOption.map(n => convertDecimalsToAccuracy(n)).getOrElse {
          require(
            precisionOption.nonEmpty,
            s"A non-nil non-fraction numeric item must have either a decimals or precision attribute")

          val precision = precisionOption.get

          convertPrecisionToAccuracy(precision, numericItemFact.text)
        }
      case f =>
        Infinity
    }
  }

  private def convertDecimalsToAccuracy(decimals: String): Accuracy = {
    if (decimals.trim == "INF") {
      Infinity
    } else {
      FiniteAccuracy(decimals.trim.toInt)
    }
  }

  private def convertPrecisionToAccuracy(precision: String, factValue: String): Accuracy = {
    precision.trim match {
      case "INF" =>
        Infinity
      case s =>
        val precisionInt = s.toInt

        if (precisionInt == 0) {
          // Not correct, but we have to return something!
          Infinity
        } else {
          val numericFactValue = BigDecimal(factValue.trim)

          if (numericFactValue.isWhole && numericFactValue.toInt == 0) {
            Infinity
          } else {
            val inferredDecimals =
              precisionInt - floor(log10(numericFactValue.abs.toDouble)).toInt - 1

            FiniteAccuracy(inferredDecimals.toInt)
          }
        }
    }
  }
}
