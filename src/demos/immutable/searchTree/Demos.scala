/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.immutable.searchTree

import dataStructures.immutable.searchTree.{BST, SearchTree}

object Demos extends App {
  var t1: SearchTree[Int] = BST[Int]()
  for (x <- List(10, 5, 15, 1, 7, 20))
    t1 = t1.insert(x)

  println(t1)

  val t2 = t1.delete(10)
  println(t2)

  val xs = t1.foldInOrder(List[Int]())(_ :: _)
  val ys = t1.foldPreOrder(List[Int]())(_ :: _)
  val zs = t1.foldPostOrder(List[Int]())(_ :: _)

  println(xs)
  println(ys)
  println(zs)


  def sum[A](t: SearchTree[A])(implicit num: Numeric[A]): A =
    t.foldInOrder(num.zero)(num.plus)

  def product[A](t: SearchTree[A])(implicit num: Numeric[A]): A =
    t.foldInOrder(num.one)(num.times)

  println(sum(t1), product(t1))

  val t3 = BST.factory[Int].empty.insert(10).insert(5)
  println(t3)

  println(t3.deleteMaxim)
  println(t3.deleteMinim)
}