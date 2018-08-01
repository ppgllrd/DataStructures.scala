/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package demos.immutable.queue

import dataStructures.immutable.queue.{LinearQueue, Queue, TwoStacksQueue}

object Demos extends App {
  var q1: Queue[Int] = LinearQueue[Int]()

  for (x <- List.range(1, 10))
    q1 = q1.enqueue(x)

  println(q1)

  var q2: Queue[Int] = LinearQueue[Int]()

  for (x <- List.range(1, 10))
    q2 = q2.enqueue(x)

  println(q2)

  println(q1 == q2)

  var q3: Queue[Int] = TwoStacksQueue[Int]()

  for (x <- List.range(1, 10))
    q3 = q3.enqueue(x)

  println(q3)

  println(q3.first)

  q3 = q3.dequeue

  println(q3)

  println(q3.first)

  q3 = q3.dequeue

  println(q3)

  println()

  var q4 = TwoStacksQueue[Int]()

  q4 = q4.enqueue(1)
  println(q4)

  q4 = q4.dequeue
  println(q4)


  println(q4.isEmpty)

  println(TwoStacksQueue.empty == TwoStacksQueue.empty)
}
