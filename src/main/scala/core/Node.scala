package core

import scala.collection.mutable.MutableList

import scala.annotation.tailrec


// TODO
// from now on, there is no root and the matchtree is the root
// the match add will determine if there is a parent or not and the current step
// think about whether this is the correct type signature or if +T will suffice
// learn more about covariance and contravariance
// 
// would be neat to have a pretty print
abstract class Node[T <: Node[T]]() extends Iterable[T] {
	//val statsInitializer : StatsInitializer
	//val matchStats = statsInitializer.initialize(this)
	//private var _alternateMatch : Option[AlternateNode] = None

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

	def allPathsWalk[T <: Node[T]](nodes: List[T]) =
		pathWalk(true, nodes)

	def allPathsWalk[T <: Node[T]](node : T) : Iterator[List[T]] =
		allPathsWalk(List(node))

	def terminalPathWalk[T <: Node[T]](node: T) : Iterator[List[T]] =
		terminalPathWalk(List(node))

	def terminalPathWalk[T <: Node[T]](nodes: List[T]) : Iterator[List[T]] =
		pathWalk(false, nodes)

	@tailrec
	private def pathWalk[T <: Node[T]](
		includeAllNodes: Boolean,
		stack : List[T],
		prefix : List[T] = Nil,
		prefixCounts : List[Int] = Nil,
		acc: Iterator[List[T]] = Iterator.empty) : Iterator[List[T]] =

		stack match {
			case head::tail => {
				val shouldDropHead = prefixCounts.headOption.map(_ == 0).getOrElse(false)

				val p = if (shouldDropHead) prefix.tail else prefix
				val pc = if (shouldDropHead) prefixCounts.tail else prefixCounts
				val pc2 = if (!pc.isEmpty) (pc.head - 1) :: pc.tail else pc

				pathWalk(
					includeAllNodes,
					head.children ++ tail,
					if (head.hasChildren) head :: p else p,
					if (head.hasChildren) head.children.size :: pc2 else pc2,
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

// alternate node will represent the first match for an alternate query
// should alternate be mutable and the reference to it be immutable?
case class AlternateNode(elementIndex : Int) {}