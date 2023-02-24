package funalgo

object KMP {

  private def buildPrefixTable(pattern: String): Vector[Int] = ???

  def find(text: String, pattern: String): Option[Int] =
    val prefixTable = buildPrefixTable(pattern)
    (0 until text.length()).foldLeft(0)((prefixIdx, currIdx) => ???)
    ???
}
