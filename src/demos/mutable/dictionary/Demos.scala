/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.mutable.dictionary

import dataStructures.mutable.dictionary.{BSTDictionary, Dictionary}

object Demos extends App {

  var d1: Dictionary[Int, String] = BSTDictionary[Int, String]()

  d1.insert(1, "One")
  d1.insert(2, "Two")
  d1.insert(0, "Zero")

  println(d1)

  d1.delete(2)

  println(d1)

  println(d1.valueOf(0))
  println(d1.valueOf(1))
  println(d1.valueOf(2))
}
