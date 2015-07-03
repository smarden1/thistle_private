package core

class MatchTreeBuilder[T](query : Query[T])(implicit val series : Vector[T]) {

	require(query.size > 0, "Query cannot be empty")

	protected val querySize = query.size

	protected val root = new MutableMatchNode(-1, -1)

	protected[core] def addStep(element : T, index : Int) : Unit = {
		(Iterator(List(root)) ++ Node.prefixWalk(root.children)).foreach{ nodeList =>
			val matchState = MatchState(nodeList.map(_.stepIndex), index)

			if (isValidMatch(matchState)) {
				nodeList.last.createAndAddChild(index)
			}
		}
	}

	def build() : MatchTree[T] = {
		series
			.zipWithIndex
			.foreach{case (element, index) => addStep(element, index)}

		new MatchTree(query, root.toImmutableNode)
	}

	def isValidMatch(matchState : MatchState[T]) : Boolean =
		query
			.lift(matchState.size)
			.map(_(matchState))
			.getOrElse(false)
}

object MatchTreeBuilder {
	def apply[T](query : Query[T])(implicit series : Vector[T]) : MatchTree[T] =
		new MatchTreeBuilder(query)(series).build
}