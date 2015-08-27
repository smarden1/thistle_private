package thistle.core

case class Query[T](
	headPredicate: ElementPredicate[T],
	tailPredicates: MatchPredicate[T]*) extends Seq[MatchPredicate[T]] {

	def length: Int =
		tailPredicates.length + 1

	def iterator: Iterator[MatchPredicate[T]] =
		Iterator(headPredicate) ++ tailPredicates.iterator

	def apply(idx: Int): MatchPredicate[T] =
		if (idx == 0) {
			headPredicate
		} else {
			tailPredicates(idx - 1)
		}
}