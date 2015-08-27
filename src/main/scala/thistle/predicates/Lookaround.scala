package thistle.predicates

import thistle.core.{MatchState, ElementState, MatchPredicate, ElementPredicate}

object Lookaround {
  // applies this given predicate and sees if it matches n places behind
  def lookBehind[T](n: Int, predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) =>
      lookAt(m.index - n, predicate)(m)

  // applies this given predicate and sees if it matches n places ahead
  def lookAhead[T](n: Int, predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) =>
      lookAt(m.index + n, predicate)(m)

  def next[T](predicate: MatchPredicate[T]): MatchPredicate[T] =
    lookAhead(1, predicate)

  def previous[T](predicate: MatchPredicate[T]): MatchPredicate[T] =
    lookBehind(1, predicate)

  //is slice a good name?
  def nextSlice[T](predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => 
      lookaround(m.index, m.size, predicate)(m)

  def nextSlice[T](n: Int, predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => 
      lookaround(m.index, m.index + n, predicate)(m)

  def previousSlice[T](n: Int, predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => 
      lookaround(m.index - n, m.index, predicate)(m)

  def previousSlice[T](predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => 
      lookaround(0, m.index, predicate)(m)

  // if there are no matches then this should be true
  def noIntervening[T](predicate: MatchPredicate[T]): MatchPredicate[T] =
    !intervening(predicate)

  def intervening[T](predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => 
      lookaround(m.previousMatchIndexes.last, m.index, predicate)(m)

  private def lookaround[T](start: Int, end: Int, predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => 
      (start to end)
        .exists(i => predicate(MatchState(m.previousMatchIndexes, i)(m.series)))

  private def lookAt[T](n: Int, predicate: MatchPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => {
      m.series.size <= n &&
        predicate(MatchState(m.previousMatchIndexes, n)(m.series))
    }
}