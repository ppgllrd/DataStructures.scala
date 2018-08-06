/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.searchTree

trait SearchTreeFactory[A] {
  def empty: SearchTree[A]
}
