package funalgo

import math.Ordered.orderingToOrdered
import scala.collection.immutable.Queue

case class BinaryTreeNode[K, V](
    key: K,
    value: V,
    left: Option[BinaryTreeNode[K, V]] = None,
    right: Option[BinaryTreeNode[K, V]] = None
)

trait BinaryTree[K, V]:
  def search(key: K): Option[V]
  def insert(key: K, value: V): BinaryTree[K, V]

class UnbalanceBinaryTree[K, V](root: BinaryTreeNode[K, V])(using Ordering[K])
    extends BinaryTree[K, V]:
  def search(key: K): Option[V] = auxSearch(key, root)

  def auxSearch(key: K, node: BinaryTreeNode[K, V]): Option[V] = key match
    case node.key => Some(node.value)
    case _ =>
      if key < node.key then node.left.flatMap(auxSearch(key, _))
      else node.right.flatMap(auxSearch(key, _))

  def insert(key: K, value: V): BinaryTree[K, V] = UnbalanceBinaryTree(
    auxInsert(key, value, root)
  )

  def auxInsert(
      key: K,
      value: V,
      currentNode: BinaryTreeNode[K, V]
  ): BinaryTreeNode[K, V] = key match
    case currentNode.key => currentNode.copy(value = value)
    case _ =>
      if key < currentNode.key then
        currentNode.copy(left =
          currentNode.left
            .map(auxInsert(key, value, _))
            .orElse(Some(BinaryTreeNode(key, value)))
        )
      else
        currentNode.copy(right =
          currentNode.right
            .map(auxInsert(key, value, _))
            .orElse(Some(BinaryTreeNode(key, value)))
        )

  def bfs(f: => (K, V) => Unit): Unit = LazyList
    .iterate(Queue(root))(q =>
      val (n, nq) = q.dequeue
      nq ++ n.left ++ n.right
    )
    .takeWhile(_.nonEmpty)
    .foreach(n =>
      n.headOption match
        case None       => ()
        case Some(node) => f(node.key, node.value)
    )

  def dfs(f: => (K, V) => Unit): Unit = auxDfs(f, root)

  private def auxDfs(f: => (K, V) => Unit, node: BinaryTreeNode[K, V]): Unit =
    node.left.foreach(auxDfs(f, _))
    f(node.key, node.value)
    node.right.foreach(auxDfs(f, _))
