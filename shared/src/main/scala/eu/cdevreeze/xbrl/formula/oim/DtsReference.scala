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

import java.net.URI

/**
 * DTS reference.
 *
 * @author Chris de Vreeze
 */
sealed trait DtsReference {

  /**
   * The absolute URI to the defining document (schema or linkbase) or element (arcrole or role).
   */
  def href: URI
}

object DtsReference {

  final case class Schema(val href: URI) extends DtsReference {
    def referenceType: String = "schema"
  }

  final case class Linkbase(val href: URI) extends DtsReference {
    def referenceType: String = "linkbase"
  }

  final case class Role(val href: URI) extends DtsReference {
    def referenceType: String = "role"
  }

  final case class Arcrole(val href: URI) extends DtsReference {
    def referenceType: String = "arcrole"
  }
}
