package thistle.core

class MatchTree[T](
  predicates: Query[T],
  val root: ImmutableMatchNode[T])(implicit private val series: Vector[T]) extends Tree[ImmutableMatchNode[T]] {

  def iterator: Iterator[Match[T]] =
    Node.terminalPathWalk(root.children).map(Match(_, predicates))

  def allMatches(): Iterator[Match[T]] =
    iterator

  def allValues(): Iterator[Seq[T]] =
    allMatches.map(_.values)

  def allIndexes(): Iterator[Seq[Int]] =
    allMatches.map(_.indexes)

  def allCompleteValues(): Iterator[Seq[T]] =
    allCompleteMatches.map(_.values)

  def allIncompleteValues(): Iterator[Seq[T]] =
    allIncompleteMatches.map(_.values)

  def allCompleteIndexes(): Iterator[Seq[Int]] =
    allCompleteMatches.map(_.indexes)

  def allIncompleteIndexes(): Iterator[Seq[Int]] =
    allIncompleteMatches.map(_.indexes)

  def allCompleteNodes(): Iterator[Seq[ImmutableMatchNode[T]]] =
    allCompleteMatches.map(_.nodes)

  def allIncompleteNodes(): Iterator[Seq[ImmutableMatchNode[T]]] =
    allIncompleteMatches.map(_.nodes)

  def allCompleteMatches(): Iterator[Match[T]] =
    iterator.filter(_.isComplete)

  def allIncompleteMatches(): Iterator[Match[T]] =
    iterator.filter(_.isIncomplete)

  def uniqueCountsPerStep(): List[Int] = {
    val emptySetMap = (0 to predicates.size).map((_ -> Set[Int]())).toMap

    iterator
      .flatMap(_.nodes)
        .foldLeft(emptySetMap)(
            (acc, node) =>
                acc + (node.stepIndex -> Set(node.elementIndex))
        )
        .mapValues(_.size)
        .toList
        .sortBy(_._1)
        .map(_._2)
    }
}

object MatchTree {
  def apply[T](query: Query[T])(implicit series: Vector[T]): MatchTree[T] =
    MatchTreeBuilder(query)(series)

  def apply[T](headPredicate: ElementPredicate[T], tailPredicates: MatchPredicate[T]*)(implicit series: Vector[T]) =
    MatchTreeBuilder(Query(headPredicate, tailPredicates: _*))(series)
}