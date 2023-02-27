package funalgo

protected class TrieHashTable[-K, V](trie: Vector[List[(K, V)]]):

  private def ###[K](key: K): Int = key.## % trie.size match
    case a if a < 0 => -a
    case a          => a

  def insert(key: K, value: V): TrieHashTable[K, V] =
    new TrieHashTable(
      trie.updated(
        ###(key),
        (key -> value) :: trie(###(key)).filter(_ != key)
      )
    )

  def search(key: K): Option[V] =
    trie.applyOrElse(###(key), _ => Nil).find(_._1 == key).map(_._2)

  def delete(key: K): TrieHashTable[K, V] = new TrieHashTable(
    trie.updated(
      ###(key),
      trie(###(key)).filter(_ != key)
    )
  )

object TrieHashTable:
  def apply[K, V](initSize: Int) =
    new TrieHashTable[K, V](Vector.fill(initSize)(Nil))
  def intString(size: Int) = apply[Int, String](size)
