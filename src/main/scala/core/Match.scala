package core

case class Match[T](
	nodes : List[ImmutableMatchNode[T]],
	predicates : Query[T])(implicit private val series : Vector[T]) {

	def isEmpty : Boolean =
		nodes.isEmpty

	def isComplete : Boolean =
		predicates.size == nodes.size

	def isIncomplete : Boolean =
		!isComplete

	def stepsCompleted : Int =
		nodes.size

	def before() : Seq[T] =
		nodes.head.nextValues()

	def after() : Seq[T] =
		nodes.last.nextValues
	
	def values() : Seq[T] =
		nodes.map(_.value)

	def valueOptions() : Seq[Option[T]] =
		values.map(Some(_)).padTo(predicates.size, None)

	def elements() : Seq[Element[T]] =
		nodes.map(_.toElement)

	def indexes() : Seq[Int] =
		nodes.map(_.index)

	def start : Int =
		nodes.head.index

	def end : Int =
		nodes.last.index

	override def toString() : String =
		"Match(%s)".format(nodes.mkString(", "))
}