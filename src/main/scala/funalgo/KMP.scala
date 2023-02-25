package funalgo

import java.util.stream.IntStream

object KMP {

  private def buildPrefixs(pattern: String): Vector[Int] = ???

  private def auxFind(
      prefixTable: Vector[Int],
      pattern: String,
      charwIdxes: List[Tuple2[Char, Int]],
      sufffixIdx: Int
  ): Option[Int] =
    charwIdxes match
      case (rune, idx) :: cdr =>
        if pattern.charAt(sufffixIdx) == rune
        then
          val nSuffixIdx = sufffixIdx + 1
          if nSuffixIdx == pattern.length() then Some(idx)
          else auxFind(prefixTable, pattern, cdr, nSuffixIdx)
        else if sufffixIdx == 0 then auxFind(prefixTable, pattern, cdr, 0)
        else
          auxFind(prefixTable, pattern, charwIdxes, prefixTable(sufffixIdx - 1))
      case Nil => None

  def find(text: String, pattern: String): Option[Int] =
    auxFind(buildPrefixs(pattern), pattern, text.zipWithIndex.toList, 0)

}
