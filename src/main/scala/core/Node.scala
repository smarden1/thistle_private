package core

import scala.collection.mutable.MutableList
import scala.annotation.tailrec

abstract class Node[T <: Node[T]]() extends Iterable[T] {
	def children : List[T]

	def hasChildren() : Boolean =
		!children.isEmpty
}

abstract class MutableNode[T <: Node[T]] extends Node[T] {
	private var _children = MutableList[T]()

	def children =
		_children.toList

	protected def addChild(node : T) : this.type = {
		_children += node
		this
	}
}

object Node {
	@tailrec
	def depthWalk[T <: Node[T]](
		stack : List[T],
		acc: Iterator[T] = Iterator.empty) : Iterator[T] =

		stack match {
			case head::tail => depthWalk(head.children ++ tail, acc ++ Iterator(head))
			case _ => acc
		}

	def depthWalk[T <: Node[T]](node : T) : Iterator[T] =
		depthWalk(node.children, Iterator(node))

	@tailrec
	def breadthWalk[T <: Node[T]](
		queue : List[T],
		acc: Iterator[T] = Iterator.empty) : Iterator[T] =

		queue match {
			case head::tail => breadthWalk(tail ++ head.children, acc ++ Iterator(head))
			case _ => acc
		}

	def breadthWalk[T <: Node[T]](node : T) : Iterator[T] =
		breadthWalk(node.children, Iterator(node))

	def allPathsWalk[T <: Node[T]](nodes: List[T]) : Iterator[List[T]] =
		pathWalk(true, nodes)

	def allPathsWalk[T <: Node[T]](node : T) : Iterator[List[T]] =
		allPathsWalk(List(node))

	def terminalPathWalk[T <: Node[T]](node: T) : Iterator[List[T]] =
		terminalPathWalk(List(node))

	def terminalPathWalk[T <: Node[T]](nodes: List[T]) : Iterator[List[T]] =
		pathWalk(false, nodes)

	@tailrec
	def pathWalk[T <: Node[T]](
		includeAllNodes: Boolean,
		stack : List[T],
		prefix : List[T] = Nil,
		prefixCounts : List[Int] = Nil,
		acc: Iterator[List[T]] = Iterator.empty) : Iterator[List[T]] =

		stack match {
			case head::tail => {
				val w = prefixCounts.indexWhere(_ > 0)
				val i = if (w == -1) prefixCounts.size else w
				val p = prefix.drop(i)
				val pc = prefixCounts.drop(i).map(_ + head.children.size - 1)

				pathWalk(
					includeAllNodes,
					head.children ++ tail,
					head :: p,
					head.children.size :: pc,
					acc ++ (if (includeAllNodes || head.children.isEmpty) Iterator((head::p).reverse) else Iterator.empty)
				)
			}
			case _ => acc
		}

	def maxDepth[T <: Node[T]](stack : List[(T, Int)]) : Int =
		stack match {
			case (node, depth)::tail => Math.max(depth, maxDepth(node.children.map((_, depth + 1)) ++ tail))
			case _ => 0
		}

	def maxDepth[T <: Node[T]](node : T) : Int =
		maxDepth(List((node, 1)))
}