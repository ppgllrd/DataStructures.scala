/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.immutable.set

import dataStructures.immutable.set.{BSTSet, Set}

object Demos extends App {
  var s1: Set[Int] = BSTSet[Int]()

  for (x <- List.range(1, 10))
    s1 = s1.insert(x)

  println(s1)

  s1 = s1.delete(4)

  println(s1)

  var s2: Set[Int] = BSTSet[Int]()

  for (x <- List.range(1, 6))
    s2 = s2.insert(x)

  println(s2)

  val s3 = s1 intersection s2

  println(s3)

  val s4 = s1 difference s2

  println(s4)
}
