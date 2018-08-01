/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.immutable.queue

trait Queue[+A] {
  def isEmpty: Boolean

  def enqueue[B >: A](x: B): Queue[B]

  def first: A

  def dequeue: Queue[A]
}
