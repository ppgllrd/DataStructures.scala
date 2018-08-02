/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.util

import org.scalacheck.{Arbitrary, Gen}

object TestData extends Enumeration {
  type Type = Value

  val A, B, C, D, E, F, G, H, I, J = Value

  implicit def arbitrary = Arbitrary[TestData.Type] {
    Gen.oneOf(TestData.values.toSeq)
  }
}