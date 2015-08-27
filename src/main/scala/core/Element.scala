package thistle.core

trait ElementLike[+T] {
	val elementIndex : Int

	implicit val series : Vector[T]

	require(
		elementIndex < series.size,
		"The elementIndex cannot be larger then the sequence length."
	)

	lazy val value: T =
		series(elementIndex)

	def isHead(): Boolean =
		isNthInSeries(0)

	def isLast(): Boolean =
		isNthInSeries(series.size - 1)

	def isNthInSeries(idx: Int): Boolean =
		elementIndex == idx

	def elementAtIndex(idx: Int): Option[Element[T]] =
		if (series.isDefinedAt(idx)) Some(Element(idx)) else None

	def offsetElement(offset: Int): Option[Element[T]] =
		elementAtIndex(elementIndex + offset) // handle negative cases

	def previousElement(): Option[Element[T]] =
		elementAtIndex(elementIndex - 1)

	def nextElement(): Option[Element[T]] =
		elementAtIndex(elementIndex + 1)

	def previousElements(): Seq[Element[T]] =
		(0 to elementIndex).map(Element(_))

	def nextElements: Seq[Element[T]] =
		(elementIndex + 1 to series.size).map(Element(_))

	def valueAtIndex(idx: Int): Option[T] =
		series.lift(idx)

	def offsetValue(offset: Int): Option[T] =
		valueAtIndex(elementIndex + offset) // handle negative cases

	def nextValue(): Option[T] =
		valueAtIndex(elementIndex + 1)

	def previousValue(): Option[T] =
		valueAtIndex(elementIndex - 1)

	def previousValues(): Seq[T] =
		series.slice(0, elementIndex)

	def nextValues(): Seq[T] =
		series.slice(elementIndex + 1, series.size)

	def toElement(): Element[T] =
		Element(elementIndex)

	def index(): Int =
		elementIndex
}

case class Element[+T](elementIndex: Int)(implicit val series: Vector[T]) extends ElementLike[T] {
	require(
		elementIndex >= 0,
		"The elementIndex cannot be a negative integer."
	)

	override def toString(): String =
		"Element(%s)".format(elementIndex)
}