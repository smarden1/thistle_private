package predicates

import core.{MatchState, MatchPredicate, ElementState}

import scala.reflect.{ClassTag, classTag}

object Matches {
	def comparePreviousMatch[T: ClassTag, U: ClassTag](prevMatchFn: (T, U) => Boolean): MatchPredicate[T] =
		(m: MatchState[T]) =>
			(m.previousMatchedValue, m.value) match {
				case (lastMatch: T, currentvalue: U) => prevMatchFn(lastMatch, currentvalue)
				case _ => false
			}
}