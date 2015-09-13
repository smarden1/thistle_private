package thistle.core

trait Tree[T <: Node[T]] {
  val root: T

  def depthWalk: Iterator[T] =
    Node.depthWalk(root.children)

  def breadthWalk: Iterator[T] =
    Node.breadthWalk(root.children)

  def maxDepth: Int =
    Math.max(Node.maxDepth(root) - 1, 0)

  def terminalPathWalk: Iterator[List[T]] =
    Node.terminalPathWalk(root.children)

  def allPathsWalk: Iterator[List[T]] =
    Node.allPathsWalk(root.children)
}