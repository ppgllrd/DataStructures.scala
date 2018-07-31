/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.immutable.stack

import dataStructures.immutable.stack.{LinearStack, Stack}

object Demos extends App {
  var s1: Stack[Int] = LinearStack[Int]()

  for (x <- List.range(1, 10))
    s1 = s1.push(x)

  println(s1)

  var s2: Stack[Int] = LinearStack[Int]()

  for (x <- List.range(1, 10))
    s2 = s2.push(x)

  println(s2)

  println(s1 == s2)
}
