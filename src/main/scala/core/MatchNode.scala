package core

trait MatchNode[T <: Node[T]] extends Node[T] {
	val elementIndex : Int

	val stepIndex : Int

	override def toString() : String =
		"MatchNode(%s, %s)".format(elementIndex, stepIndex)
}

case class MutableMatchNode(elementIndex : Int, stepIndex : Int)
	extends MutableNode[MutableMatchNode]
	with MatchNode[MutableMatchNode] {
	
	def createAndAddChild(elementIndex : Int) : this.type =
		addChild(MutableMatchNode(elementIndex, stepIndex + 1))

	// IS THIS RIGHT?
	def walk : Stream[MutableMatchNode] =
		Node.depthWalk(children)

	def toImmutableNode[T](implicit series : Vector[T]) : ImmutableMatchNode[T] = {
		this match {
			case _ : Root[MutableMatchNode] => new ImmutableMatchNode(elementIndex, stepIndex, children.map(_.toImmutableNode)) with Root[ImmutableMatchNode[T]]
			case _ => new ImmutableMatchNode(elementIndex, stepIndex, children.map(_.toImmutableNode))
		}
	}
}

// is this poorly named? with series here?
case class ImmutableMatchNode[T](
	elementIndex : Int,
	stepIndex : Int,
	children : List[ImmutableMatchNode[T]] = Nil)(implicit val series : Vector[T])
	extends MatchNode[ImmutableMatchNode[T]]
	with ElementLike[T] {

	def walks : Iterator[ImmutableMatchNode[T]] =
		Node.dWalk(this)

	def walk : Stream[ImmutableMatchNode[T]] =
		Node.depthWalk(children)
}