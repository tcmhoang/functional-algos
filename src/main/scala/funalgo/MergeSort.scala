package funalgo

object MergeSort:
  def sort[T](input: Seq[T])(using Ordering[T]): List[T] =
    input match
      case Nil      => Nil
      case _ :: Nil => input.toList
      case _ =>
        input.toList.splitAt(input.length / 2) match
          case (l, r) => merge(sort(l), sort(r), Nil)

  private def merge[T](l: List[T], r: List[T], acc: List[T])(using
      order: Ordering[T]
  ): List[T] =
    (l, r) match
      case (Nil, Nil)      => Nil
      case (Nil, ele :: _) => acc.reverse ++ r
      case (ele :: _, Nil) => acc.reverse ++ l
      case (le :: ls, re :: rs) =>
        if order.compare(le, re) > 0 then merge(l, rs, re :: acc)
        else merge(ls, r, le :: acc)
@main def run() =
  println(MergeSort.sort(List(2, 1, 3)))
