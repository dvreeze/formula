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
 * Report, so the "XBRL instance". According to the OIM, the DTS references and facts are unordered,
 * but they are stored here in the order in which they would occur in the corresponding XBRL XML document.
 *
 * This is therefore not a case class, and has no value equality!
 *
 * @author Chris de Vreeze
 */
final class Report(
  dtsReferences: immutable.IndexedSeq[DtsReference],
  topLevelFacts: immutable.IndexedSeq[Fact])
