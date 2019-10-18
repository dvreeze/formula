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

import scala.collection.immutable
import scala.math.floor
import scala.math.log10

import eu.cdevreeze.tqa.base.queryapi.TaxonomyApi
import eu.cdevreeze.tqa.ENames
import eu.cdevreeze.tqa.Namespaces
import eu.cdevreeze.tqa.instance
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path
import eu.cdevreeze.xbrl.formula.oim.Accuracy
import eu.cdevreeze.xbrl.formula.oim.ArcroleReference
import eu.cdevreeze.xbrl.formula.oim.AspectValue
import eu.cdevreeze.xbrl.formula.oim.AspectValueSet
import eu.cdevreeze.xbrl.formula.oim.ConceptAspectValue
import eu.cdevreeze.xbrl.formula.oim.EntityAspectValue
import eu.cdevreeze.xbrl.formula.oim.ExplicitDimensionAspectValue
import eu.cdevreeze.xbrl.formula.oim.Fact
import eu.cdevreeze.xbrl.formula.oim.FiniteAccuracy
import eu.cdevreeze.xbrl.formula.oim.Forever
import eu.cdevreeze.xbrl.formula.oim.LanguageAspectValue
import eu.cdevreeze.xbrl.formula.oim.LinkbaseReference
import eu.cdevreeze.xbrl.formula.oim.LocalTimeInterval
import eu.cdevreeze.xbrl.formula.oim.NonNumericSimpleFact
import eu.cdevreeze.xbrl.formula.oim.NumericSimpleFact
import eu.cdevreeze.xbrl.formula.oim.PeriodAspectValue
import eu.cdevreeze.xbrl.formula.oim.PeriodValue
import eu.cdevreeze.xbrl.formula.oim.Report
import eu.cdevreeze.xbrl.formula.oim.RoleReference
import eu.cdevreeze.xbrl.formula.oim.SchemaReference
import eu.cdevreeze.xbrl.formula.oim.SimpleFact
import eu.cdevreeze.xbrl.formula.oim.SimpleValue
import eu.cdevreeze.xbrl.formula.oim.TupleFact
import eu.cdevreeze.xbrl.formula.oim.TupleOrderAspectValue
import eu.cdevreeze.xbrl.formula.oim.TupleParentAspectValue
import eu.cdevreeze.xbrl.formula.oim.TypedDimensionAspectValue
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

    val aspectValueSetsByContextId: Map[String, Set[AspectValue]] =
      xbrlInstance.findAllContexts
        .groupBy(_.id)
        .view.mapValues(ctxs => extractAspectValuesFromContext(ctxs.head)).toMap

    val aspectValueSetsByUnitId: Map[String, Set[AspectValue]] =
      xbrlInstance.findAllUnits
        .groupBy(_.id)
        .view.mapValues(ctxs => Set[AspectValue](extractUnitAspectValueFromUnit(ctxs.head))).toMap

    val langOption = xbrlInstance.attributeOption(ENames.XmlLangEName)

    val topLevelFacts = findAllTopLevelFactsWithPathEntries(xbrlInstance)
      .map {
        case (fact, pathEntry) =>
          createOimFact(
            fact,
            Path(immutable.IndexedSeq(pathEntry)),
            None,
            langOption,
            aspectValueSetsByContextId,
            aspectValueSetsByUnitId)
      }

    Report.build(dtsReferences, topLevelFacts)
  }

  // TODO Footnotes

  // Private methods

  private def convertSchemaRef(schemaRef: instance.SchemaRef): SchemaReference = {
    SchemaReference(schemaRef.resolvedHref)
  }

  private def convertLinkbaseRef(schemaRef: instance.LinkbaseRef): LinkbaseReference = {
    LinkbaseReference(schemaRef.resolvedHref)
  }

  private def convertRoleRef(schemaRef: instance.RoleRef): RoleReference = {
    RoleReference(schemaRef.resolvedHref)
  }

  private def convertArcroleRef(schemaRef: instance.ArcroleRef): ArcroleReference = {
    ArcroleReference(schemaRef.resolvedHref)
  }

  private def createOimFact(
    fact: instance.Fact,
    ownPath: Path,
    zeroBasedOrderOption: Option[Int],
    inheritedLanguageOption: Option[String],
    aspectValueSetsByContextId: Map[String, Set[AspectValue]],
    aspectValueSetsByUnitId: Map[String, Set[AspectValue]]): Fact = {

    fact match {
      case itemFact: instance.ItemFact =>
        createOimSimpleFact(
          itemFact,
          ownPath,
          zeroBasedOrderOption,
          inheritedLanguageOption,
          aspectValueSetsByContextId,
          aspectValueSetsByUnitId)
      case tupleFact: instance.TupleFact =>
        createOimTupleFact(
          tupleFact,
          ownPath,
          zeroBasedOrderOption,
          inheritedLanguageOption,
          aspectValueSetsByContextId,
          aspectValueSetsByUnitId)
    }
  }

  private def createOimSimpleFact(
    fact: instance.ItemFact,
    ownPath: Path,
    zeroBasedOrderOption: Option[Int],
    inheritedLanguageOption: Option[String],
    aspectValueSetsByContextId: Map[String, Set[AspectValue]],
    aspectValueSetsByUnitId: Map[String, Set[AspectValue]]): SimpleFact = {

    fact match {
      case nonNumericItemFact: instance.NonNumericItemFact =>
        createOimNonNumericSimpleFact(
          nonNumericItemFact,
          ownPath,
          zeroBasedOrderOption,
          inheritedLanguageOption,
          aspectValueSetsByContextId,
          aspectValueSetsByUnitId)
      case numericItemFact: instance.NumericItemFact =>
        createOimNumericSimpleFact(
          numericItemFact,
          ownPath,
          zeroBasedOrderOption,
          inheritedLanguageOption,
          aspectValueSetsByContextId,
          aspectValueSetsByUnitId)
    }
  }

  private def createOimTupleFact(
    fact: instance.TupleFact,
    ownPath: Path,
    zeroBasedOrderOption: Option[Int],
    inheritedLanguageOption: Option[String],
    aspectValueSetsByContextId: Map[String, Set[AspectValue]],
    aspectValueSetsByUnitId: Map[String, Set[AspectValue]]): TupleFact = {

    val idOption = fact.attributeOption(ENames.IdEName)

    val conceptAspectValue = ConceptAspectValue(fact.resolvedName)
    val tupleParentAspectValue = TupleParentAspectValue(ownPath.parentPath)
    val tupleOrderAspectValue = TupleOrderAspectValue(zeroBasedOrderOption)

    val aspectValues = Set[AspectValue](conceptAspectValue, tupleParentAspectValue, tupleOrderAspectValue)

    val langOption = fact.attributeOption(ENames.XmlLangEName).orElse(inheritedLanguageOption)

    // Recursive calls into createOimFact

    val childFacts = findAllChildFactsWithPathEntries(fact).zipWithIndex.map {
      case ((childFact, pathEntry), idx) =>
        createOimFact(
          childFact,
          ownPath.append(pathEntry),
          Some(idx),
          langOption,
          aspectValueSetsByContextId,
          aspectValueSetsByUnitId)
    }

    TupleFact.from(
      idOption,
      AspectValueSet.from(aspectValues),
      childFacts)
  }

  private def createOimNonNumericSimpleFact(
    fact: instance.NonNumericItemFact,
    ownPath: Path,
    zeroBasedOrderOption: Option[Int],
    inheritedLanguageOption: Option[String],
    aspectValueSetsByContextId: Map[String, Set[AspectValue]],
    aspectValueSetsByUnitId: Map[String, Set[AspectValue]]): NonNumericSimpleFact = {

    val idOption = fact.attributeOption(ENames.IdEName)

    val aspectValuesFromContext =
      aspectValueSetsByContextId.getOrElse(fact.contextRef, sys.error(s"Missing context with ID '${fact.contextRef}'"))

    val conceptAspectValue = ConceptAspectValue(fact.resolvedName)
    val tupleParentAspectValue = TupleParentAspectValue(ownPath.parentPath)
    val tupleOrderAspectValue = TupleOrderAspectValue(zeroBasedOrderOption)

    val langOption = fact.attributeOption(ENames.XmlLangEName).orElse(inheritedLanguageOption)
    val languageAspectValue = LanguageAspectValue(langOption)

    val aspectValues =
      Set[AspectValue](
        conceptAspectValue,
        tupleParentAspectValue,
        tupleOrderAspectValue,
        languageAspectValue).union(aspectValuesFromContext)

    val factValueOption = extractNonNumericFactValueOption(fact)

    NonNumericSimpleFact.from(
      idOption,
      AspectValueSet.from(aspectValues),
      factValueOption)
  }

  private def createOimNumericSimpleFact(
    fact: instance.NumericItemFact,
    ownPath: Path,
    zeroBasedOrderOption: Option[Int],
    inheritedLanguageOption: Option[String],
    aspectValueSetsByContextId: Map[String, Set[AspectValue]],
    aspectValueSetsByUnitId: Map[String, Set[AspectValue]]): NumericSimpleFact = {

    // TODO Require a non-fraction numeric item

    val idOption = fact.attributeOption(ENames.IdEName)

    val aspectValuesFromContext =
      aspectValueSetsByContextId.getOrElse(fact.contextRef, sys.error(s"Missing context with ID '${fact.contextRef}'"))

    val aspectValuesFromUnit =
      aspectValueSetsByUnitId.getOrElse(fact.unitRef, sys.error(s"Missing unit with ID '${fact.unitRef}'"))

    val conceptAspectValue = ConceptAspectValue(fact.resolvedName)
    val tupleParentAspectValue = TupleParentAspectValue(ownPath.parentPath)
    val tupleOrderAspectValue = TupleOrderAspectValue(zeroBasedOrderOption)

    val aspectValues =
      Set[AspectValue](
        conceptAspectValue,
        tupleParentAspectValue,
        tupleOrderAspectValue).union(aspectValuesFromContext).union(aspectValuesFromUnit)

    val factValueOption = extractNumericFactValueOption(fact)

    val accuracy = extractAccuracy(fact)

    NumericSimpleFact.from(
      idOption,
      AspectValueSet.from(aspectValues),
      factValueOption,
      accuracy)
  }

  private def extractAspectValuesFromContext(context: instance.XbrliContext): Set[AspectValue] = {
    val entityAspectValue = extractEntityAspectValueFromContext(context)
    val periodAspectValue = extractPeriodAspectValueFromContext(context)

    val explicitDimensionAspectValues = extractExplicitDimensionAspectValuesFromContext(context)
    val typedDimensionAspectValues = extractTypedDimensionAspectValuesFromContext(context)

    Set[AspectValue](entityAspectValue, periodAspectValue)
      .union(explicitDimensionAspectValues.collect { case av: AspectValue => av })
      .union(typedDimensionAspectValues.collect { case av: AspectValue => av })
  }

  private def extractUnitAspectValueFromUnit(unit: instance.XbrliUnit): UnitAspectValue = {
    val numerators = unit.numeratorMeasures
    val denominators = unit.denominatorMeasures

    UnitAspectValue(numerators.toSet, denominators.toSet)
  }

  private def extractEntityAspectValueFromContext(context: instance.XbrliContext): EntityAspectValue = {
    val scheme = URI.create(context.entity.identifierScheme)
    val identifier = context.entity.identifierValue

    EntityAspectValue(scheme, identifier)
  }

  private def extractPeriodAspectValueFromContext(context: instance.XbrliContext): PeriodAspectValue = {
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

  private def extractExplicitDimensionAspectValuesFromContext(context: instance.XbrliContext): Set[ExplicitDimensionAspectValue] = {
    context.explicitDimensionMembers.toSeq
      .map {
        case (dim, mem) =>
          ExplicitDimensionAspectValue(dim, mem)
      }
      .toSet
  }

  private def extractTypedDimensionAspectValuesFromContext(context: instance.XbrliContext): Set[TypedDimensionAspectValue] = {
    context.typedDimensionMembers.toSeq
      .map {
        case (dim, mem) =>
          val convertedMember = extractTypedDimensionMember(dim, mem)

          TypedDimensionAspectValue(dim, convertedMember)
      }
      .toSet
  }

  private def extractTypedDimensionMember(dimension: EName, memberElem: instance.XbrliElem): SimpleValue = {
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

    val memberText = memberElem.text

    SimpleValue.parse(memberText, baseTypeOption.getOrElse(ENames.XsStringEName))
  }

  private def extractNonNumericFactValueOption(itemFact: instance.NonNumericItemFact): Option[SimpleValue] = {
    if (itemFact.isNil) {
      None
    } else {
      val itemDecl = dts.getItemDeclaration(itemFact.resolvedName)

      // TODO Note that if an anonymous type is used, it is ignored here! Fix this!

      val typeOption: Option[EName] = itemDecl.globalElementDeclaration.typeOption

      // TODO Stop at xbrli base type, and turn that into its built-in schema base type.

      val baseTypeOption: Option[EName] =
        typeOption.flatMap { tp =>
          dts.findBaseTypeOrSelfUntil(tp, _.namespaceUriOption.contains(Namespaces.XsNamespace))
        }

      val rawFactValue = itemFact.text

      val simpleValue =
        SimpleValue.parse(rawFactValue, baseTypeOption.getOrElse(ENames.XsStringEName))
      Some(simpleValue)
    }
  }

  private def extractNumericFactValueOption(itemFact: instance.NumericItemFact): Option[SimpleValue] = {
    if (itemFact.isNil) {
      None
    } else {
      val itemDecl = dts.getItemDeclaration(itemFact.resolvedName)

      // TODO Note that if an anonymous type is used, it is ignored here! Fix this!

      val typeOption: Option[EName] = itemDecl.globalElementDeclaration.typeOption

      // TODO Stop at xbrli base type, and turn that into its built-in schema base type.

      val baseTypeOption: Option[EName] =
        typeOption.flatMap { tp =>
          dts.findBaseTypeOrSelfUntil(tp, _.namespaceUriOption.contains(Namespaces.XsNamespace))
        }

      val rawFactValue = itemFact.text

      val simpleValue =
        SimpleValue.parse(rawFactValue, baseTypeOption.getOrElse(ENames.XsStringEName))
      Some(simpleValue)
    }
  }

  private def findAllTopLevelFactsWithPathEntries(
    xbrlInstance: instance.XbrlInstance): immutable.IndexedSeq[(instance.Fact, Path.Entry)] = {

    var entryMap = Map[EName, Int]()

    xbrlInstance.findAllTopLevelFacts.map { fact =>
      val factName = fact.resolvedName
      val idx = entryMap.getOrElse(factName, 0)
      entryMap += factName -> (idx + 1)
      (fact, Path.Entry(factName, idx))
    }
  }

  private def findAllChildFactsWithPathEntries(
    fact: instance.TupleFact): immutable.IndexedSeq[(instance.Fact, Path.Entry)] = {

    var entryMap = Map[EName, Int]()

    fact.findAllChildFacts.map { fact =>
      val factName = fact.resolvedName
      val idx = entryMap.getOrElse(factName, 0)
      entryMap += factName -> (idx + 1)
      (fact, Path.Entry(factName, idx))
    }
  }

  /**
   * Extracts the accuracy from the given numeric item fact. See section 4.6.6 of the XBRL specification.
   */
  private def extractAccuracy(numericItemFact: instance.NumericItemFact): Accuracy = {
    numericItemFact match {
      case f: instance.NilNumericItemFact =>
        Accuracy.Infinity
      case f: instance.NonNilFractionItemFact =>
        Accuracy.Infinity
      case f: instance.NonNilNonFractionNumericItemFact =>
        val decimalsOption = f.decimalsOption
        val precisionOption = f.precisionOption

        decimalsOption.map(n => Accuracy.parse(n.toString)).getOrElse {
          require(
            precisionOption.nonEmpty,
            s"A non-nil non-fraction numeric item must have either a decimals or precision attribute")

          val precision = precisionOption.get

          convertPrecisionToAccuracy(precision, numericItemFact.text)
        }
      case f =>
        Accuracy.Infinity
    }
  }

  private def convertPrecisionToAccuracy(precision: String, factValue: String): Accuracy = {
    val precisionAsAccuracy = Accuracy.parse(precision.trim)

    precisionAsAccuracy match {
      case Accuracy.Infinity =>
        Accuracy.Infinity
      case FiniteAccuracy(precisionInt) =>
        if (precisionInt == 0) {
          // Not correct, but we have to return something!
          Accuracy.Infinity
        } else {
          val numericFactValue = BigDecimal(factValue.trim)

          if (numericFactValue.isWhole && numericFactValue.toInt == 0) {
            Accuracy.Infinity
          } else {
            val inferredDecimals =
              precisionInt - floor(log10(numericFactValue.abs.toDouble)).toInt - 1

            FiniteAccuracy(inferredDecimals.toInt)
          }
        }
    }
  }
}
