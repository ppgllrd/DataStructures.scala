/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.mutable.searchTree

import dataStructures.mutable.searchTree.{BST, SearchTree}

object Demos extends App {
  var t1: SearchTree[Int] = BST[Int]()
  for (x <- List(10, 5, 15, 1, 7, 20))
    t1.insert(x)

  println(t1)

  for (x <- t1.inOrder)
    println(x)

  println()
  for (x <- t1.preOrder)
    println(x)

  t1.delete(10)
  println(t1)

  t1.delete(7)
  println(t1)

  t1.deleteMinim
  println(t1)

  t1.deleteMaxim
  println(t1)

  t1.deleteMaxim
  println(t1)

  t1.deleteMinim
  println(t1)


  val factory = BST.factory[Int]

  val t2 = factory.empty
  for (x <- List(10, 5, 15, 1, 7, 20))
    t2.insert(x)

  println(t2)
}