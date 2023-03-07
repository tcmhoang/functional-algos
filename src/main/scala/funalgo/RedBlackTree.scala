package funalgo
import math.Ordering.Implicits.infixOrderingOps
import scala.collection.immutable.Queue

enum Color:
  case Red, Black

case class RedBlackNode[K, V](
    key: K,
    value: V,
    color: Color = Color.Red,
    left: Option[RedBlackNode[K, V]] = None,
    right: Option[RedBlackNode[K, V]] = None
)

class RedBlackTree[K, V](root: RedBlackNode[K, V])(using Ordering[K])
    extends BinaryTree[K, V] {

  override def search(key: K): Option[V] = ???

  private def rRotate(parent: RedBlackNode[K, V]): RedBlackNode[K, V] =
    parent.left match
      case None       => parent
      case Some(node) => node.copy(right = Some(parent.copy(left = node.right)))

  private def lRotate(parent: RedBlackNode[K, V]): RedBlackNode[K, V] =
    parent.right match
      case None       => parent
      case Some(node) => node.copy(left = Some(parent.copy(right = node.left)))

  def insert(key: K, value: V): RedBlackTree[K, V] = new RedBlackTree(
    auxInsert(key, value, root.copy(color = Color.Black))
  )

  def auxInsert(
      key: K,
      value: V,
      currentNode: RedBlackNode[K, V]
  ): RedBlackNode[K, V] = key match
    case currentNode.key => currentNode.copy(value = value)
    case _ =>
      if key < currentNode.key then
        val nl = currentNode.left
          .map(auxInsert(key, value, _))
          .orElse(Some(RedBlackNode(key, value)))
        val gn = currentNode.copy(left = nl)
        fixrb(gn, nl.get, gn.right).getOrElse(gn)
      else
        val nr = currentNode.right
          .map(auxInsert(key, value, _))
          .orElse(Some(RedBlackNode(key, value)))
        val gn = currentNode.copy(right = nr)
        fixrb(gn, nr.get, gn.left).getOrElse(gn)

  private def fixrb(
      gn: RedBlackNode[K, V],
      pn: RedBlackNode[K, V],
      un: Option[RedBlackNode[K, V]]
  ): Option[RedBlackNode[K, V]] =
    ((pn.color match
      case Color.Red =>
        (pn.right :: pn.left :: Nil)
          .find(_.map(_.color == Color.Red).getOrElse(false))
          .flatMap(identity)
      case Color.Black => None
    ) match
      case None => None
      case node: Some[RedBlackNode[K, V]] =>
        (
          node,
          un match
            case None => Color.Black
            case Some(unv) =>
              unv.color
        ) match
          case (_, Color.Red) =>
            Some(
              if pn == gn.left then
                gn.copy(
                  left = Some(pn.copy(color = Color.Black)),
                  right = un.map(_.copy(color = Color.Black)),
                  color = Color.Red
                )
              else
                gn.copy(
                  right = Some(pn.copy(color = Color.Black)),
                  left = un.map(_.copy(color = Color.Black)),
                  color = Color.Red
                )
            )
          case (node, Color.Black) =>
            Some(
              if pn == gn.left then
                rRotate(
                  gn.copy(
                    color = Color.Red,
                    left = Some(if node == pn.right then lRotate(pn) else pn)
                      .map(_.copy(color = Color.Black))
                  )
                )
              else
                lRotate(
                  gn.copy(
                    color = Color.Red,
                    right = Some(if node == pn.left then rRotate(pn) else pn)
                      .map(_.copy(color = Color.Black))
                  )
                )
            )
    )

  def foreachBFS(f: (K, V) => Unit): Unit =
    val sq = Stream.iterate(Queue(root)) { q =>
      val (node, tail) = q.dequeue
      tail ++ List(node.left, node.right).flatten
    }
    sq.takeWhile(q => q.nonEmpty).foreach(q => f(q.head.key, q.head.value))

}

object RedBlackTree:
  def apply[K, V](key: K, value: V)(using Ordering[K]) =
    new RedBlackTree[K, V](RedBlackNode[K, V](key, value))

object App {
  def main(args: Array[String]): Unit = {
    val x = RedBlackTree(1, "A")
      .insert(2, "B")
      .insert(3, "C")
      .insert(4, "D")
      .insert(5, "E")
      .insert(6, "F")
      .insert(7, "G")
      .insert(8, "H")
      .insert(9, "I")
      .insert(10, "J")

    x.foreachBFS((k, v) => println(s"$k -> $v"))
  }
}
