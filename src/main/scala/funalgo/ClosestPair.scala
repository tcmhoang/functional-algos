package funalgo

import java.util.concurrent.ThreadLocalRandom

case class Point(x: Int, y: Int):
  def distanceTo(pt: Point): Double =
    Math.sqrt(Math.pow(x - pt.x, 2) + Math.pow(y - pt.y, 2))

object ClosestPair {
  def generatePairs(num: Int): List[Point] = (0 until num)
    .map(_ =>
      Point(
        ThreadLocalRandom.current().nextInt(),
        ThreadLocalRandom.current().nextInt()
      )
    )
    .toList

  def closestDistance(
      ptsSortedX: List[Point],
      ptsSortedY: List[Point]
  ): Double =
    if ptsSortedX.size <= 3 then closestDistanceBF(ptsSortedX)
    else
      val (lx, rx) = ptsSortedX.splitAt(ptsSortedX.size / 2)
      val dX = rx.head.x
      val (ly, ry) = ptsSortedY.partition(_.x < dX)
      val delta = closestDistance(lx, ly) min closestDistance(rx, ry)
      val ptsWithinBoundary =
        ptsSortedY.filter(p => p.x >= dX - delta && p.x <= dX + delta)
      LazyList
        .iterate(ptsWithinBoundary)(_.tail)
        .takeWhile(_.nonEmpty)
        .foldLeft(delta)((cur, pts) =>
          val possiblePts = pts.takeWhile(p => p.y <= pts.head.y + delta)
          cur min (if possiblePts.size > 1 then closestDistanceBF(possiblePts)
                   else Int.MaxValue)
        )

  def closestDistanceBF(pts: List[Point]) =
    (for
      (pt, i) <- pts.zipWithIndex.dropRight(1)
      pj <- pts.drop(i + 1)
    yield pt.distanceTo(pj)).min

}

@main def main(): Unit = {
  val allPoints = ClosestPair.generatePairs(3000)

  val before = System.currentTimeMillis()
  val distanceBruteForce = ClosestPair.closestDistanceBF(allPoints)
  val after = System.currentTimeMillis()

  val before1 = System.currentTimeMillis()
  val distance =
    ClosestPair.closestDistance(allPoints.sortBy(_.x), allPoints.sortBy(_.y))
  val after1 = System.currentTimeMillis()

  println(
    s"Closest Point using Brute Force is $distanceBruteForce, found in ${after - before} ms"
  )
  println(
    s"Closest Point using Divide & Conquer is $distance, found in ${after1 - before1} ms"
  )

}
