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

import org.scalatest.FunSuite

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
      report.topLevelFacts.size
    }

    assertResult(Vector("m1", "m2", "m3", "m4").map(s => EName(ns, s))) {
      report.topLevelFacts.map(_.conceptName)
    }

    val firstFact = report.topLevelFacts(0)

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
          .withUnit(UnitAspectValue.fromNumerators(Set(EName(Iso4217Namespace, "JPY")))),
        Some(NumericValue(BigDecimal(10000))),
        Accuracy.Infinity)

    assertResult(expectedFirstFact) {
      firstFact
    }

    val lastFact = report.topLevelFacts.last

    val expectedLastFact =
      NumericSimpleFact.from(
        None,
        AspectValueSet.Empty
          .withConcept(EName(ns, "m4"))
          .withEntity(URI.create("http://xbrl.org/entity/identification/scheme"), "DDD004")
          .withPeriod(LocalTimeInterval.fromLocalDate(LocalDate.of(2007, 4, 4)))
          .withUnit(UnitAspectValue.fromNumerators(Set(EName(Iso4217Namespace, "TOP")))),
        Some(NumericValue(BigDecimal(40))),
        Accuracy.Infinity)

    assertResult(expectedLastFact) {
      lastFact
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

    val nestedFacts = report.topLevelTupleFacts.flatMap(_.descendantFacts)

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
