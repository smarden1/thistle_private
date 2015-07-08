package core

class MatchTree[T](predicates : Query[T], val root : ImmutableMatchNode[T])(implicit private val series : Vector[T]) {

	def iterator : Iterator[Match[T]] =
		Node.terminalPathWalk(root.children).map(Match(_, predicates))

	def findAllMatches() : Iterator[Match[T]] =
		iterator

	def findAllValues() : Iterator[Seq[T]] =
		findAllMatches.map(_.values)

	def findAllIndexes() : Iterator[Seq[Int]] =
		findAllMatches.map(_.indexes)

	def findAllCompleteValues() : Iterator[Seq[T]] =
		findAllCompleteMatches.map(_.values)

	def findAllIncompleteValues() : Iterator[Seq[T]] =
		findAllIncompleteMatches.map(_.values)

	def findAllCompleteIndexes() : Iterator[Seq[Int]] =
		findAllCompleteMatches.map(_.indexes)

	def findAllIncompleteIndexes() : Iterator[Seq[Int]] =
		findAllIncompleteMatches.map(_.indexes)

	def findAllCompleteNodes() : Iterator[Seq[ImmutableMatchNode[T]]] =
		findAllCompleteMatches.map(_.nodes)

	def findAllIncompleteNodes() : Iterator[Seq[ImmutableMatchNode[T]]] =
		findAllIncompleteMatches.map(_.nodes)

	def findAllCompleteMatches() : Iterator[Match[T]] =
		iterator.filter(_.isComplete)

	def findAllIncompleteMatches() : Iterator[Match[T]] =
		iterator.filter(_.isIncomplete)

	def uniqueCountsPerStep() : List[Int] = {
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
	def apply[T](query : Query[T])(implicit series : Vector[T]) : MatchTree[T] =
		MatchTreeBuilder(query)(series)

	def apply[T](headPredicate: ElementPredicate[T], tailPredicates : MatchPredicate[T]*)(implicit series : Vector[T]) =
		MatchTreeBuilder(Query(headPredicate, tailPredicates:_*))(series)
}