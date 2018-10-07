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
import eu.cdevreeze.xbrl.formula.oim._
import net.sf.saxon.s9api.Processor

/**
 * Formula simulation test case, using only the OIM model. It uses test data from the XBRL Formula conformance suite.
 *
 * @author Chris de Vreeze
 */
class FormulaSimulationTest extends FunSuite {

  private val Iso4217Namespace = "http://www.xbrl.org/2003/iso4217"

  test("test-12010-v-01") {
    val parentDir = URI.create("conformance-formula-2018-09-13/10000%20Formula/12010-Formula-Processing-ConstructingFactValue/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("12010-just-a-number-instance.xml")

    val xbrlInstance: XbrlInstance =
      XbrlInstanceDocument.build(docBuilder.build(instanceUri)).documentElement

    val taxo =
      makeTestDts(
        xbrlInstance.findAllSchemaRefs.map(_.resolvedHref).toSet
          .union(xbrlInstance.findAllLinkbaseRefs.map(_.resolvedHref).toSet))

    val oimMapper = new XmlToOimMapper(taxo)

    val report = oimMapper.convertXbrlInstance(xbrlInstance)

    val ns = "http://xbrl.org/formula/conformance/example"

    // Simulate formula in 12010-just-a-number-formula.xml

    // Use (fast) concept name filters first.
    // Now we probably have a small sequence of fact values for these fact variables to process.

    val n1Facts = report.findNumericSimpleFactsByName(EName(ns, "n1"))

    val resultFacts: immutable.IndexedSeq[NumericSimpleFact] =
      for {
        n1Fact <- n1Facts
      } yield {
        NumericSimpleFact.from(
          None,
          n1Fact.aspectValueSet,
          Some(NumericValue(BigDecimal(1.2))),
          FiniteAccuracy(1))
      }

    val expectedResultFacts =
      List(
        NumericSimpleFact.from(
          None,
          AspectValueSet.Empty
            .withConcept(EName(ns, "n1"))
            .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "01")
            .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 12, 31)))
            .withUnit(Set(EName(Namespaces.XbrliNamespace, "pure"))),
          Some(NumericValue(BigDecimal(1.2))),
          FiniteAccuracy(1)))

    assertResult(expectedResultFacts) {
      resultFacts
    }
  }

  test("test-12010-v-08") {
    val parentDir = URI.create("conformance-formula-2018-09-13/10000%20Formula/12010-Formula-Processing-ConstructingFactValue/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("12010-empty-intersection-instance.xml")

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

    // Simulate formula in 12010-intersection-formula.xml

    val uncoveredAspects =
      aspectUniverse.filterNot(_.isInstanceOf[DimensionAspect]).diff(Set(ConceptAspect, PeriodAspect))

    // Use (fast) concept name filter for factVar1 etc., and use the knowledge that it matches only numeric simple facts.
    // Now we probably have a small sequence of fact value sequences for this fact variable to process.

    val n1FactSeqs: Map[AspectValueSet, immutable.IndexedSeq[NumericSimpleFact]] =
      report.findNumericSimpleFactsByName(EName(ns, "n1"))
        .groupBy(_.aspectValueSet.filteringAspects(uncoveredAspects))

    val n2FactSeqs: Map[AspectValueSet, immutable.IndexedSeq[NumericSimpleFact]] =
      report.findNumericSimpleFactsByName(EName(ns, "n2"))
        .groupBy(_.aspectValueSet.filteringAspects(uncoveredAspects))

    val n3Facts = report.findNumericSimpleFactsByName(EName(ns, "n3"))

    val resultFacts: immutable.IndexedSeq[NumericSimpleFact] =
      for {
        (aspectValueSet1, n1FactSeq) <- n1FactSeqs.toIndexedSeq
        (aspectValueSet2, n2FactSeq) <- n2FactSeqs.toIndexedSeq
        if aspectValueSet1.filteringAspects(uncoveredAspects) ==
          aspectValueSet2.filteringAspects(uncoveredAspects)

        n3Fact <- n3Facts
        if aspectValueSet1.filteringAspects(uncoveredAspects) ==
          n3Fact.aspectValueSet.filteringAspects(uncoveredAspects)
      } yield {
        val overlappingValues =
          n1FactSeq.flatMap(_.factValueOption).toSet.intersect(n2FactSeq.flatMap(_.factValueOption).toSet)
            .map(_.asBigDecimal)
        val resultValueOption: Option[BigDecimal] =
          if (overlappingValues.size == 1) Some(overlappingValues.head) else None

        NumericSimpleFact.from(
          None,
          n3Fact.aspectValueSet,
          resultValueOption.map(v => NumericValue(v)),
          FiniteAccuracy(0))
      }

    val expectedResultFacts =
      List(
        NumericSimpleFact.from(
          None,
          AspectValueSet.Empty
            .withConcept(EName(ns, "n3"))
            .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "01")
            .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 12, 31)))
            .withUnit(Set(EName(Namespaces.XbrliNamespace, "pure"))),
          None,
          FiniteAccuracy(0)))

    assertResult(expectedResultFacts) {
      resultFacts
    }
  }

  test("test-12010-v-09") {
    val parentDir = URI.create("conformance-formula-2018-09-13/10000%20Formula/12010-Formula-Processing-ConstructingFactValue/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("12010-an-intersection-instance.xml")

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

    // Simulate formula in 12010-intersection-formula.xml, but this time producing no nil fact

    val uncoveredAspects =
      aspectUniverse.filterNot(_.isInstanceOf[DimensionAspect]).diff(Set(ConceptAspect, PeriodAspect))

    // Use (fast) concept name filter for factVar1 etc., and use the knowledge that it matches only numeric simple facts.
    // Now we probably have a small sequence of fact value sequences for this fact variable to process.

    val n1FactSeqs: Map[AspectValueSet, immutable.IndexedSeq[NumericSimpleFact]] =
      report.findNumericSimpleFactsByName(EName(ns, "n1"))
        .groupBy(_.aspectValueSet.filteringAspects(uncoveredAspects))

    val n2FactSeqs: Map[AspectValueSet, immutable.IndexedSeq[NumericSimpleFact]] =
      report.findNumericSimpleFactsByName(EName(ns, "n2"))
        .groupBy(_.aspectValueSet.filteringAspects(uncoveredAspects))

    val n3Facts = report.findNumericSimpleFactsByName(EName(ns, "n3"))

    val resultFacts: immutable.IndexedSeq[NumericSimpleFact] =
      for {
        (aspectValueSet1, n1FactSeq) <- n1FactSeqs.toIndexedSeq
        (aspectValueSet2, n2FactSeq) <- n2FactSeqs.toIndexedSeq
        if aspectValueSet1.filteringAspects(uncoveredAspects) ==
          aspectValueSet2.filteringAspects(uncoveredAspects)

        n3Fact <- n3Facts
        if aspectValueSet1.filteringAspects(uncoveredAspects) ==
          n3Fact.aspectValueSet.filteringAspects(uncoveredAspects)
      } yield {
        val overlappingValues =
          n1FactSeq.flatMap(_.factValueOption).toSet.intersect(n2FactSeq.flatMap(_.factValueOption).toSet)
            .map(_.asBigDecimal)
        val resultValueOption: Option[BigDecimal] =
          if (overlappingValues.size == 1) Some(overlappingValues.head) else None

        NumericSimpleFact.from(
          None,
          n3Fact.aspectValueSet,
          resultValueOption.map(v => NumericValue(v)),
          FiniteAccuracy(0))
      }

    val expectedResultFacts =
      List(
        NumericSimpleFact.from(
          None,
          AspectValueSet.Empty
            .withConcept(EName(ns, "n3"))
            .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "01")
            .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 12, 31)))
            .withUnit(Set(EName(Namespaces.XbrliNamespace, "pure"))),
          Some(NumericValue(BigDecimal(13))),
          FiniteAccuracy(0)))

    assertResult(expectedResultFacts) {
      resultFacts
    }
  }

  test("test-12010-v-12") {
    val parentDir = URI.create("conformance-formula-2018-09-13/10000%20Formula/12010-Formula-Processing-ConstructingFactValue/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("12010-sum-instance.xml")

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

    // Simulate formula in 12010-sum-formula.xml

    val uncoveredAspects =
      aspectUniverse.filterNot(_.isInstanceOf[DimensionAspect]).diff(Set(ConceptAspect, PeriodAspect))

    // Use (fast) concept name filter for factVar1, and use the knowledge that it matches only numeric simple facts.
    // Now we probably have a small sequence of fact value sequences for this fact variable to process.

    val n1FactSeqs: Map[AspectValueSet, immutable.IndexedSeq[NumericSimpleFact]] =
      report.findNumericSimpleFactsByName(EName(ns, "n1"))
        .groupBy(_.aspectValueSet.filteringAspects(uncoveredAspects))

    val n3Facts = report.findNumericSimpleFactsByName(EName(ns, "n3"))

    val resultFacts: immutable.IndexedSeq[NumericSimpleFact] =
      for {
        (aspectValueSet, n1FactSeq) <- n1FactSeqs.toIndexedSeq
        n3Fact <- n3Facts

        if aspectValueSet.filteringAspects(uncoveredAspects) ==
          n3Fact.aspectValueSet.filteringAspects(uncoveredAspects)
      } yield {
        NumericSimpleFact.from(
          None,
          n3Fact.aspectValueSet,
          Some(NumericValue(n1FactSeq.map(_.factValueOption.get.asBigDecimal).sum)),
          FiniteAccuracy(0))
      }

    val expectedResultFacts =
      List(
        NumericSimpleFact.from(
          None,
          AspectValueSet.Empty
            .withConcept(EName(ns, "n3"))
            .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "01")
            .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 12, 31)))
            .withUnit(Set(EName(Namespaces.XbrliNamespace, "pure"))),
          Some(NumericValue(BigDecimal(10000))),
          FiniteAccuracy(0)))

    assertResult(expectedResultFacts) {
      resultFacts
    }
  }

  test("test-12020-v-01") {
    val parentDir = URI.create("conformance-formula-2018-09-13/10000%20Formula/12020-Formula-Processing-AspectRules/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("12020-nearest-source-instance.xml")

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

    // Simulate formula in 12020-nearest-source-formula.xml

    val uncoveredAspects =
      aspectUniverse.diff(Set(ConceptAspect, PeriodAspect, UnitAspect, EntityAspect))

    // Use (fast) concept name filters first.
    // Now we probably have a small sequence of fact values for these fact variables to process.

    val m1Facts = report.findNumericSimpleFactsByName(EName(ns, "m1"))
    val m2Facts = report.findNumericSimpleFactsByName(EName(ns, "m2"))
    val m3Facts = report.findNumericSimpleFactsByName(EName(ns, "m3"))
    val m4Facts = report.findNumericSimpleFactsByName(EName(ns, "m4"))

    val resultFacts: immutable.IndexedSeq[NumericSimpleFact] =
      for {
        m1Fact <- m1Facts

        m2Fact <- m2Facts
        if m1Fact.aspectValueSet.filteringAspects(uncoveredAspects) ==
          m2Fact.aspectValueSet.filteringAspects(uncoveredAspects)

        m3Fact <- m3Facts
        if m2Fact.aspectValueSet.filteringAspects(uncoveredAspects) ==
          m3Fact.aspectValueSet.filteringAspects(uncoveredAspects)

        m4Fact <- m4Facts
        if m3Fact.aspectValueSet.filteringAspects(uncoveredAspects) ==
          m4Fact.aspectValueSet.filteringAspects(uncoveredAspects)
      } yield {
        NumericSimpleFact.from(
          None,
          m1Fact.aspectValueSet
            .withEntity(m1Fact.entityAspectValue.scheme, "ABC123")
            .withPeriod(m2Fact.periodAspectValue)
            .withUnit(m4Fact.unitAspectValue),
          Some(NumericValue(
            m1Fact.factValueOption.get.asBigDecimal +
              m2Fact.factValueOption.get.asBigDecimal +
              m3Fact.factValueOption.get.asBigDecimal +
              m4Fact.factValueOption.get.asBigDecimal)),
          m1Fact.accuracy)
      }

    val expectedResultFacts =
      List(
        NumericSimpleFact.from(
          None,
          AspectValueSet.Empty
            .withConcept(EName(ns, "m1"))
            .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "ABC123")
            .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 2, 2)))
            .withUnit(Set(EName(Iso4217Namespace, "TOP"))),
          Some(NumericValue(BigDecimal(12340))),
          Accuracy.Infinity))

    assertResult(expectedResultFacts) {
      resultFacts
    }
  }

  test("test-33210-v-08") {
    val parentDir = URI.create("conformance-formula-2018-09-13/30000%20Assertions/33210-ValueAssertion-Processing/")
    val parentDirUri = dummyUriPrefix.resolve(parentDir)
    val instanceUri: URI = parentDirUri.resolve("33210-seq-dupTest-1-instance.xml")

    val xbrlInstance: XbrlInstance =
      XbrlInstanceDocument.build(docBuilder.build(instanceUri)).documentElement

    val taxo =
      makeTestDts(
        xbrlInstance.findAllSchemaRefs.map(_.resolvedHref).toSet
          .union(xbrlInstance.findAllLinkbaseRefs.map(_.resolvedHref).toSet))

    val oimMapper = new XmlToOimMapper(taxo)

    val report = oimMapper.convertXbrlInstance(xbrlInstance)

    val aspectUniverse = report.aspectUniverse

    // Simulate formula in 33210-seq-dupTest-1-formula.xml

    val uncoveredAspects = aspectUniverse

    val factSeqs: Map[AspectValueSet, immutable.IndexedSeq[SimpleFact]] =
      report.findAllSimpleFacts
        .groupBy(_.aspectValueSet.filteringAspects(uncoveredAspects))

    val evalResults: immutable.IndexedSeq[Boolean] =
      for {
        (aspectValueSet, factSeq) <- factSeqs.toIndexedSeq
        if factSeq.size > 1
      } yield {
        val allEqual = factSeq.flatMap(_.factValueOption).distinct.size == 1
        allEqual
      }

    assertResult(6) {
      evalResults.size
    }
    assertResult(1) {
      evalResults.filter(Set(true)).size
    }
  }

  test("test-49210-v-01") {
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
    val otherRootDir = new File(classOf[FormulaSimulationTest].getResource("/xbrl-and-w3").toURI)
    val zipFile = new File(classOf[FormulaSimulationTest].getResource("/conformance-formula-2018-09-13.zip").toURI)

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
