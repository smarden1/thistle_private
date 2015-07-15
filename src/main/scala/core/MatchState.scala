package core

case class MatchState[+T](
	previousMatchIndexes : Seq[Int],
	override val elementIndex : Int)(implicit override val series : Vector[T]) extends ElementState[T](elementIndex) {

	lazy val previousMatchValues : Seq[T] =
		previousMatchIndexes.map(series(_))

	def previousMatches() : Seq[Element[T]] =
		previousMatchIndexes.map(Element(_))

	def hasPreviousMatches : Boolean =
		!previousMatches.isEmpty

	def previousMatchedIndex : Int =
		previousMatchIndexes.last

	// need to guard for being the first
	def previousMatchedValue : T =
		series(previousMatchedIndex)

	def previousMatchedElement : Element[T] =
		Element(previousMatchedIndex)

	def size : Int =
		previousMatchIndexes.size

	def toElementState() : ElementState[T] =
		ElementState(elementIndex)

	def changeElementIndex(i : Int) : MatchState[T] =
		MatchState(previousMatchIndexes, i)(series)

	override def toString() : String =
		"MatchState(%s, %s)".format(previousMatchIndexes, elementIndex)
}

// no previous matches
class ElementState[+T](
	val elementIndex : Int)(implicit val series : Vector[T]) extends ElementLike[T] {

	lazy val element =
		this.toElement

	def toMatchState() : MatchState[T] =
		MatchState(Nil, elementIndex)
}

object ElementState {
	def apply[T](elementIndex : Int)(implicit series : Vector[T]) : ElementState[T] =
		new ElementState(elementIndex)
}