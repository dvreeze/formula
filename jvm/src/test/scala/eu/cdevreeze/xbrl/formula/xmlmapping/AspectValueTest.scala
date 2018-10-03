/*
 * Copyright 2011-2017 Chris de Vreeze
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

import java.io.File
import java.net.URI
import java.time.LocalDate
import java.util.zip.ZipFile

import scala.collection.immutable
import scala.math.BigDecimal

import org.scalatest.FunSuite

import eu.cdevreeze.tqa.Namespaces
import eu.cdevreeze.tqa.base.relationship.DefaultRelationshipFactory
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy
import eu.cdevreeze.tqa.base.taxonomybuilder.DefaultDtsCollector
import eu.cdevreeze.tqa.base.taxonomybuilder.TaxonomyBuilder
import eu.cdevreeze.tqa.docbuilder.SimpleCatalog
import eu.cdevreeze.tqa.docbuilder.jvm.PartialUriResolvers
import eu.cdevreeze.tqa.docbuilder.jvm.UriResolvers
import eu.cdevreeze.tqa.docbuilder.saxon.SaxonDocumentBuilder
import eu.cdevreeze.tqa.instance.XbrlInstance
import eu.cdevreeze.tqa.instance.XbrlInstanceDocument
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path
import eu.cdevreeze.xbrl.formula.oim._
import net.sf.saxon.s9api.Processor

/**
 * Aspect value querying test case. It uses test data from the XBRL Formula conformance suite.
 *
 * @author Chris de Vreeze
 */
class AspectValueTest extends FunSuite {

  private val Iso4217Namespace = "http://www.xbrl.org/2003/iso4217"

  test("testConvertSimpleInstanceToOimReport") {
    val parentDir = URI.create("conformance-formula-2018-09-13/10000%20Formula/12030-Formula-Processing-ConceptRules/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("12030-dynamic-concept-instance.xml")

    val xbrlInstance: XbrlInstance =
      XbrlInstanceDocument.build(docBuilder.build(instanceUri)).documentElement

    val taxo =
      makeTestDts(
        xbrlInstance.findAllSchemaRefs.map(_.resolvedHref).toSet
          .union(xbrlInstance.findAllLinkbaseRefs.map(_.resolvedHref).toSet))

    val oimMapper = new XmlToOimMapper(taxo)

    val report = oimMapper.convertXbrlInstance(xbrlInstance)

    val ns = "http://xbrl.org/formula/conformance/example"

    assertResult(xbrlInstance.findAllTopLevelFacts.size) {
      report.findAllTopLevelFacts.size
    }

    assertResult(Vector("m1", "m2", "m3", "m4").map(s => EName(ns, s))) {
      report.findAllTopLevelFacts.map(_.conceptName)
    }

    val firstFact = report.findAllTopLevelFacts(0)

    assertResult(true) {
      firstFact.isInstanceOf[NumericSimpleFact]
    }

    val firstNumericFact = firstFact.asInstanceOf[NumericSimpleFact]

    assertResult(ConceptAspectValue(EName(ns, "m1"))) {
      firstNumericFact.conceptAspectValue
    }

    assertResult(EntityAspectValue(URI.create("http://xbrl.org/entity/identification/scheme"), "AAA001")) {
      firstNumericFact.entityAspectValue
    }

    assertResult(PeriodAspectValue(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 1, 1)))) {
      firstNumericFact.periodAspectValue
    }

    assertResult(UnitAspectValue(Set(EName(Iso4217Namespace, "JPY")), Set())) {
      firstNumericFact.unitAspectValue
    }

    assertResult(Set.empty) {
      firstNumericFact.dimensionAspectValues
    }

    val expectedFirstFact =
      NumericSimpleFact.from(
        None,
        AspectValueSet.Empty
          .withConcept(EName(ns, "m1"))
          .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "AAA001")
          .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 1, 1)))
          .withUnit(Set(EName(Iso4217Namespace, "JPY"))),
        Some(NumericValue(BigDecimal(10000))),
        Accuracy.Infinity)

    assertResult(expectedFirstFact) {
      firstFact
    }

    val lastFact = report.findAllTopLevelFacts.last

    val expectedLastFact =
      NumericSimpleFact.from(
        None,
        AspectValueSet.Empty
          .withConcept(EName(ns, "m4"))
          .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "DDD004")
          .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 4, 4)))
          .withUnit(Set(EName(Iso4217Namespace, "TOP"))),
        Some(NumericValue(BigDecimal(40))),
        Accuracy.Infinity)

    assertResult(expectedLastFact) {
      lastFact
    }

    assertResult(1) {
      report.findFactsByName(EName(ns, "m4")).size
    }

    assertResult(1) {
      report.findSimpleFactsByPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 4, 4))).size
    }
  }

  test("testTupleParentAspect") {
    val parentDir = URI.create("conformance-formula-2018-09-13/40000%20Filters/51210-TupleFilter-Processing-Parent/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("51210-parentInTuple-instance.xml")

    val xbrlInstance: XbrlInstance =
      XbrlInstanceDocument.build(docBuilder.build(instanceUri)).documentElement

    val taxo =
      makeTestDts(
        xbrlInstance.findAllSchemaRefs.map(_.resolvedHref).toSet
          .union(xbrlInstance.findAllLinkbaseRefs.map(_.resolvedHref).toSet))

    val oimMapper = new XmlToOimMapper(taxo)

    val report = oimMapper.convertXbrlInstance(xbrlInstance)

    val ns = "http://xbrl.org/formula/conformance/example"

    val nestedFacts = report.findAllTopLevelTupleFacts.flatMap(_.findAllDescendantFacts)

    assertResult(2) {
      nestedFacts.size
    }
    assertResult(List(EName(ns, "b"), EName(ns, "c"))) {
      nestedFacts.map(_.conceptName)
    }

    val parentPath = Path.from(EName(ns, "t") -> 0)

    assertResult(List(TupleParentAspectValue(parentPath), TupleParentAspectValue(parentPath))) {
      nestedFacts.map(_.tupleParentAspectValue)
    }
    assertResult(List(TupleOrderAspectValue(Some(0)), TupleOrderAspectValue(Some(1)))) {
      nestedFacts.map(_.tupleOrderAspectValue)
    }

    assertResult(nestedFacts) {
      nestedFacts
        .map(f => (f.tupleParentAspectValue.parentPath, f.tupleOrderAspectValue.zeroBasedOrderOption))
        .map {
          case (tuplePath, orderInTuple) =>
            report.getNestedFact(tuplePath, orderInTuple.getOrElse(1000000))
        }
    }
  }

  test("testSimulatedFormula") {
    val parentDir = URI.create("conformance-formula-2018-09-13/40000%20Filters/49210-RelativeFilter-Processing-Dimensional/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("49210-restatementFilter-instance.xml")

    val xbrlInstance: XbrlInstance =
      XbrlInstanceDocument.build(docBuilder.build(instanceUri)).documentElement

    val taxo =
      makeTestDts(
        xbrlInstance.findAllSchemaRefs.map(_.resolvedHref).toSet
          .union(xbrlInstance.findAllLinkbaseRefs.map(_.resolvedHref).toSet))

    val oimMapper = new XmlToOimMapper(taxo)

    val report = oimMapper.convertXbrlInstance(xbrlInstance)

    val aspectUniverse = report.aspectUniverse

    val ns = "http://xbrl.org/formula/conformance/example"

    // Simulate formula in 49210-restatementFilter-formula.xml

    // Use (fast) concept name filter for factVarStock, and use the knowledge that it matches only numeric simple facts.
    // Now we probably have a small sequence of fact values for this fact variable to process.

    val stockFacts = report.findNumericSimpleFactsByName(EName(ns, "stock"))

    // Use (fast) concept name filter for factVarFlow, knowing that it matches only numeric simple facts.
    // Now we probably have a small sequence of fact values for this fact variable to process.

    val flowFacts = report.findNumericSimpleFactsByName(EName(ns, "flow"))

    // Note that implicit filtering is switched off

    // Now use the other 3 filters for factVarStock

    val restatementAxisAspect = TypedDimensionAspect(EName(ns, "restatementAxis"))

    val resultFacts: immutable.IndexedSeq[NumericSimpleFact] =
      for {
        stockFact <- stockFacts
        flowFact <- flowFacts

        if flowFact.periodAspectValue.isFiniteDuration &&
          stockFact.periodAspectValue.isInstant &&
          flowFact.periodAspectValue.asTimeInterval.start == stockFact.periodAspectValue.asTimeInterval.end

        if stockFact.findTypedDimensionAspectValue(restatementAxisAspect.dimension).map(_.member).exists(_.nonEmpty)
        if stockFact.findTypedDimensionAspectValue(restatementAxisAspect.dimension)
          .map(v => LocalTimeInterval.fromLocalDate(LocalDate.parse(v.member.value.toString))).contains(
            flowFact.periodAspectValue.asTimeInterval.atEnd)

        uncoveredAspects = aspectUniverse.diff(Set(ConceptAspect, PeriodAspect, restatementAxisAspect))
        if stockFact.aspectValueSet.filteringAspects(uncoveredAspects) ==
          flowFact.aspectValueSet.filteringAspects(uncoveredAspects)
      } yield {
        NumericSimpleFact.from(
          None,
          stockFact.aspectValueSet
            .withPeriod(flowFact.periodAspectValue.asTimeInterval.atEnd),
          Some(NumericValue(
            stockFact.factValueOption.get.asBigDecimal +
              flowFact.factValueOption.get.asBigDecimal)),
          FiniteAccuracy(0))
      }

    assertResult(2) {
      resultFacts.size
    }
    assertResult(List(EName(ns, "stock"), EName(ns, "stock"))) {
      resultFacts.map(_.conceptName)
    }
    assertResult(List(BigDecimal(145), BigDecimal(169))) {
      resultFacts.map(_.factValueOption.map(_.asBigDecimal).getOrElse(BigDecimal(0)))
    }
    assertResult(List(Set(EName(Namespaces.XbrliNamespace, "pure")), Set(EName(Namespaces.XbrliNamespace, "pure")))) {
      resultFacts.map(_.unitAspectValue.numerators)
    }

    val expectedResultFacts =
      List(
        NumericSimpleFact.from(
          None,
          AspectValueSet.Empty
            .withConcept(EName(ns, "stock"))
            .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "01")
            .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 6, 30)))
            .withUnit(Set(EName(Namespaces.XbrliNamespace, "pure")))
            .withTypedDimension(restatementAxisAspect.dimension, StringValue(LocalDate.of(2007, 6, 30).toString)),
          Some(NumericValue(BigDecimal(145))),
          FiniteAccuracy(0)),
        NumericSimpleFact.from(
          None,
          AspectValueSet.Empty
            .withConcept(EName(ns, "stock"))
            .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "01")
            .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 12, 31)))
            .withUnit(Set(EName(Namespaces.XbrliNamespace, "pure")))
            .withTypedDimension(restatementAxisAspect.dimension, StringValue(LocalDate.of(2007, 12, 31).toString)),
          Some(NumericValue(BigDecimal(169))),
          FiniteAccuracy(0)))

    assertResult(expectedResultFacts) {
      resultFacts
    }
  }

  private def makeTestDts(entryPointUris: Set[URI]): BasicTaxonomy = {
    val documentCollector = DefaultDtsCollector()

    val relationshipFactory = DefaultRelationshipFactory.StrictInstance

    val taxoBuilder =
      TaxonomyBuilder.
        withDocumentBuilder(docBuilder).
        withDocumentCollector(documentCollector).
        withRelationshipFactory(relationshipFactory)

    val basicTaxo = taxoBuilder.build(entryPointUris)
    basicTaxo
  }

  private val processor = new Processor(false)

  private val dummyUriPrefix: URI = URI.create("http://www.example.com/")

  private val docBuilder: SaxonDocumentBuilder = {
    val otherRootDir = new File(classOf[AspectValueTest].getResource("/xbrl-and-w3").toURI)
    val zipFile = new File(classOf[AspectValueTest].getResource("/conformance-formula-2018-09-13.zip").toURI)

    val xbrlAndW3UriPartialResolver = PartialUriResolvers.fromLocalMirrorRootDirectory(otherRootDir)

    val catalog =
      SimpleCatalog(
        None,
        Vector(SimpleCatalog.UriRewrite(None, dummyUriPrefix.toString, "")))

    val zipFilePartialResolver = PartialUriResolvers.forZipFileUsingCatalog(new ZipFile(zipFile), catalog)

    SaxonDocumentBuilder(
      processor.newDocumentBuilder(),
      UriResolvers.fromPartialUriResolversWithFallback(
        Vector(zipFilePartialResolver, xbrlAndW3UriPartialResolver)))
  }
}
