package thistle.predicates

import thistle.core.{ElementState, ElementPredicate, MatchState, MatchPredicate}

object Lookaround {
  // applies this given predicate and sees if it matches n places behind
  def lookBehind[T](n: Int, predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) =>
      lookAt(m.index - n, predicate)(m)

  // applies this given predicate and sees if it matches n places ahead
  def lookAhead[T](n: Int, predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) =>
      lookAt(m.index + n, predicate)(m)

  def next[T](predicate: ElementPredicate[T]): ElementPredicate[T] =
    lookAhead(1, predicate)

  def previous[T](predicate: ElementPredicate[T]): ElementPredicate[T] =
    lookBehind(1, predicate)

  def nextSlice[T](predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) =>
      lookaround(m.index, m.series.size, predicate)(m)

  def nextSlice[T](n: Int, predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) =>
      lookaround(m.index, m.index + n, predicate)(m)

  def previousSlice[T](n: Int, predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) => 
      lookaround(m.index - n, m.index, predicate)(m)

  def previousSlice[T](predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) =>
      lookaround(0, m.index, predicate)(m)

  def noIntervening[T](predicate: ElementPredicate[T]): MatchPredicate[T] =
    !intervening(predicate)

  def intervening[T](predicate: ElementPredicate[T]): MatchPredicate[T] =
    (m: MatchState[T]) => 
      lookaround(m.previousMatchIndexes.last, m.index, predicate)(m)

  private def lookaround[T](start: Int, end: Int, predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) =>
      (start to end)
        .exists(i => predicate(ElementState(i)(m.series)))

  private def lookAt[T](n: Int, predicate: ElementPredicate[T]): ElementPredicate[T] =
    (m: ElementState[T]) => {
      m.series.size <= n &&
        predicate(ElementState(n)(m.series))
    }
}