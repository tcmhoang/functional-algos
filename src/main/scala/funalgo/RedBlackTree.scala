package funalgo

enum Color:
  case Red, Black

case class RedBlackNode[K, V](
    key: K,
    value: V,
    color: Color = Color.Black,
    left: Option[RedBlackNode[K, V]],
    right: Option[RedBlackNode[K, V]]
)

class RedBlackTree[K, V](root: RedBlackNode[K, V])(using Ordering[K])
    extends BinaryTree[K, V] {

  override def search(key: K): Option[V] = ???

  override def insert(key: K, value: V): BinaryTree[K, V] = ???

  private def rRotate(parent: RedBlackNode[K, V]): RedBlackNode[K, V] =
    parent.left match
      case None       => parent
      case Some(node) => node.copy(right = Some(parent.copy(left = node.right)))

  private def lRotate(parent: RedBlackNode[K, V]): RedBlackNode[K, V] =
    parent.right match
      case None       => parent
      case Some(node) => node.copy(left = Some(parent.copy(right = node.left)))
}
