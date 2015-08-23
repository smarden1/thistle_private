package core

class MatchTreeBuilder[T](query: Query[T])(implicit private val series: Vector[T]) {

	require(query.size > 0, "Query cannot be empty")

	private val querySize = query.size

	private val root = new MutableMatchNode(-1, -1)

	private def addStep(index: Int): Unit = {
		Node.allPathsWalk(root.children).foreach{ nodeList =>
			val matchState = MatchState(nodeList.map(_.elementIndex), index)

			if (isValidMatch(matchState)) {
				nodeList.last.createAndAddChild(index)
			}
		}

		if (isValidMatch(MatchState(Nil, index))) {
			root.createAndAddChild(index)
		}
	}

	private[core] def isValidMatch(matchState: MatchState[T]): Boolean =
		query
			.lift(matchState.size)
			.map(_(matchState))
			.getOrElse(false)

	def build(): MatchTree[T] = {
		series
			.zipWithIndex
			.foreach{case (element, index) => addStep(index)}

		new MatchTree(query, root.toImmutableNode)
	}
}

object MatchTreeBuilder {
	def apply[T](query: Query[T])(implicit series: Vector[T]): MatchTree[T] =
		new MatchTreeBuilder(query)(series).build
}