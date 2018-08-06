/** ****************************************************************************
  * Data Structures in Scala
  *
  * Pepe Gallardo, 2018
  * ****************************************************************************/

package dataStructures.mutable.dictionary

trait DictionaryFactory[K, V] {
  def empty: Dictionary[K, V]
}
