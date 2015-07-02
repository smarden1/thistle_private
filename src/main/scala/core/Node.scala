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
abstract class Node[T <: Node[T]]() extends Seq[T] {
	//val statsInitializer : StatsInitializer
	//val matchStats = statsInitializer.initialize(this)
	//private var _alternateMatch : Option[AlternateNode] = None

	def walk : Stream[T] // this here is weird to me. would like to have stackable traits for breadth or depth

	def children : List[T]

	def hasChildren() : Boolean =
		!children.isEmpty

	def apply(idx : Int) : T =
		walk(idx)

	def iterator : Iterator[T] =
		walk.iterator

	def length : Int =
		walk.length
}

trait Root[T <: Node[T]] extends Node[T] {}

object Root {
	implicit def root2NodeList[T <: Node[T]](root : Root[T]) : List[T] =
		root.children
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

// make this a trait eh?
// or just put it all on node
object Node {
	def dWalk[T <: Node[T]](node : T) : Iterator[T] = {
		@tailrec
		def _dWalk[T <: Node[T]](stack : List[T], acc: Iterator[T]) : Iterator[T] =
			stack match {
				case head::tail =>  _dWalk(head.children ++ tail, Iterator(head) ++ acc)
				case _ => Iterator.empty
			}
		_dWalk(node.children, Iterator(node))
	}

	def depthWalk[T <: Node[T]](stack : List[T]) : Stream[T] =
		stack match {
			case head::tail => head #:: depthWalk(head.children ++ tail)
			case _ => Stream.empty
		}

	def depthWalk[T <: Node[T]](node : T) : Stream[T] =
		node #:: depthWalk(node.children)

	// to rewrite with tco i need an accumulator? is his call in the final position?
	def breadthWalk[T <: Node[T]](queue : List[T]) : Stream[T] = 
		queue match {
			case head::tail => head #:: breadthWalk(tail ++ head.children)
			case _ => Stream.empty
		}

	def breadthWalk[T <: Node[T]](node : T) : Stream[T] =
		node #:: breadthWalk(node.children)

	// todo - make this cleaner
	// is this actually faster then just traversing up through parents
	// TAIL RECURSION?
	// TODO rename this and private
	def _prefixWalk[T <: Node[T]](stack : List[T], prefix : List[T] = Nil, prefixCounts : List[Int] = Nil) : Stream[List[T]] =
		stack match {
			case head::tail => {
				val shouldDropHead = prefixCounts.headOption.map(_ == 0).getOrElse(false)
				
				val p = if (shouldDropHead) prefix.tail else prefix
				val pc = if (shouldDropHead) prefixCounts.tail else prefixCounts
				val pc2 = if (!pc.isEmpty) (pc.head - 1) :: pc.tail else pc

				(head::p) #:: 
					_prefixWalk(
						head.children ++ tail,
						if (head.hasChildren) head :: p else p,
						if (head.hasChildren) head.children.size :: pc2 else pc2
					)
			}
			case _ => Stream.empty
		}

	def headOrChildren[T <: Node[T]](node: T): List[T] =
		node match {
			case _ : Root[_] => node.children
			case _ => List(node)
		}

	def prefixWalk[T <: Node[T]](node : T) : Stream[List[T]] =
		_prefixWalk(headOrChildren(node))

	def maxDepth[T <: Node[T]](stack : List[(T, Int)]) : Int =
		stack match {
			case (node, depth)::tail => Math.max(depth, maxDepth(node.children.map((_, depth + 1)) ++ tail))
			case _ => 0
		}

	def maxDepth[T <: Node[T]](node : T) : Int =
		maxDepth(List((node, 1)))
}


// for testing
case class SimpleNode(label : String) extends MutableNode[SimpleNode]() {
	def createAndAddChild(label : String) : SimpleNode = {
		val node = SimpleNode(label)
		addChild(node)
		node
	}

	def walk() : Stream[SimpleNode] =
		Node.depthWalk(this)

	override def toString() : String =
		"<%s>".format(label)
}

// alternate node will represent the first match for an alternate query
// should alternate be mutable and the reference to it be immutable?
case class AlternateNode(elementIndex : Int) {}