package predicates

import core.{MatchState, ElementState, MatchPredicateImplicits, MatchPredicate, ElementPredicate}

// this is a stupid name but is basically predicates that use indexes
object Indexes {

	val first : ElementPredicate[Any] =
		(m : ElementState[_]) => m.isHead

	val last : ElementPredicate[Any] =
		(m : ElementState[_]) => m.isLast

	val even : ElementPredicate[Any] =
		(m : ElementState[_]) => m.elementIndex % 2 == 0

	val odd =
		!even

	def index(idx: Int) : ElementPredicate[Any] =
		(m: ElementState[_]) => m.elementIndex == idx

	val consecutive : MatchPredicate[Any] =
		(m : MatchState[_]) => 
			!m.hasPreviousMatches || (m.previousMatchIndexes.last == m.index - 1)

	// if there are no matches then this should be true
	// if function return true then it is intervening
	def noIntervening[T](fn: T => Boolean) : MatchPredicate[T] =
		(m : MatchState[T]) => 
			!m
				.series
				.slice(m.previousMatchIndexes.last, m.index)
				.exists(fn)

	def intervening[T](fn: T => Boolean, atLeast: Int = 1) : MatchPredicate[T] =
		(m : MatchState[T]) => 
			atLeast <= m
				.series
				.slice(m.previousMatchIndexes.last, m.index)
				.count(fn)

	def intervening[T](n: Int): MatchPredicate[T] =
		intervening((_) => true, n)

	// intervening of type?
	// some sort of lookahead?

	def withinN[T](n: Int) : MatchPredicate[T] =
		(m : MatchState[T]) =>
			!m.hasPreviousMatches || (m.index - m.previousMatchIndexes.last >= n)


}
