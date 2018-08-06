/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.mutable.stack

import dataStructures.mutable.stack.{ArrayStack, LinkedStack, Stack}

object Demos extends App {
  val s1: Stack[Int] = ArrayStack(10)

  for (x <- List.range(1, 50))
    s1.push(x)

  println(s1)


  val s2: Stack[Int] = LinkedStack()

  for (x <- List.range(1, 10))
    s2.push(x)

  println(s2)


  val factory = ArrayStack.factory[Int](5)

  val s3: Stack[Int] = factory.empty

  for (x <- List.range(1, 10))
    s3.push(x)

  println(s3)
}
