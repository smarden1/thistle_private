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

	def iterator : Iterator[MutableMatchNode] =
		Node.depthWalk(this)

	def toImmutableNode[T](implicit series : Vector[T]) : ImmutableMatchNode[T] =
		new ImmutableMatchNode(elementIndex, stepIndex, children.map(_.toImmutableNode))
}

case class ImmutableMatchNode[T](
	elementIndex : Int,
	stepIndex : Int,
	children : List[ImmutableMatchNode[T]] = Nil)(implicit val series : Vector[T])
	extends MatchNode[ImmutableMatchNode[T]]
	with ElementLike[T] {

	def iterator : Iterator[ImmutableMatchNode[T]] =
		Node.depthWalk(this)
}