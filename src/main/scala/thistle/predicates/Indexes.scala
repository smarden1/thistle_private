package thistle.predicates

import thistle.core.{MatchState, ElementState, MatchPredicate, ElementPredicate}

object Indexes {

	val first: ElementPredicate[Any] =
		(m: ElementState[_]) =>
			m.isHead

	val last: ElementPredicate[Any] =
		(m: ElementState[_]) =>
			m.isLast

	val even: ElementPredicate[Any] =
		(m: ElementState[_]) =>
			m.elementIndex % 2 == 0

	val odd =
		!even

	def atIndex(idx: Int): ElementPredicate[Any] =
		(m: ElementState[_]) =>
			m.elementIndex == idx

	val consecutive: MatchPredicate[Any] =
		(m: MatchState[_]) => 
			m.previousMatchIndexes.last == m.index - 1
}
