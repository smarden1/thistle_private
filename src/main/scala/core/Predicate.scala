package core

trait MatchPredicate[-T] {
	def apply(matchState : MatchState[T]) : Boolean

	def unary_! : MatchPredicate[T] =
		AnonymousMatchPredicate( (matchState : MatchState[T]) => !this(matchState))

	def && [U <: T](that : MatchPredicate[U]) : MatchPredicate[U] =
		AnonymousMatchPredicate( (matchState : MatchState[U]) => this(matchState) && that(matchState))

	def || [U <: T](that : MatchPredicate[U]) : MatchPredicate[U] =
		AnonymousMatchPredicate( (matchState : MatchState[U]) => this(matchState) || that(matchState))

	def - [U <: T](that : MatchPredicate[U]) : MatchPredicate[U] =
		AnonymousMatchPredicate( (matchState : MatchState[U]) => this(matchState) && !that(matchState))

	def ^ [U <: T](that : MatchPredicate[U]) : MatchPredicate[U] =
		AnonymousMatchPredicate( (matchState : MatchState[U]) => (this(matchState) || that(matchState) && !(this(matchState) && that(matchState))))
}

trait ElementPredicate[-T] extends MatchPredicate[T] {
	def apply(elementState: ElementState[T]) : Boolean

	def apply(matchState : MatchState[T]) : Boolean =
		this(matchState.toElementState)

	override def unary_! : ElementPredicate[T] =
		AnonymousElementPredicate( (matchState : ElementState[T]) => !this(matchState))

	def && [U <: T](that : ElementPredicate[U]) : ElementPredicate[U] =
		AnonymousElementPredicate( (matchState : ElementState[U]) => this(matchState) && that(matchState))

	def || [U <: T](that : ElementPredicate[U]) : ElementPredicate[U] =
		AnonymousElementPredicate( (matchState : ElementState[U]) => this(matchState) || that(matchState))

	def - [U <: T](that : ElementPredicate[U]) : ElementPredicate[U] =
		AnonymousElementPredicate( (matchState : ElementState[U]) => this(matchState) && !that(matchState))

	def ^ [U <: T](that : ElementPredicate[U]) : ElementPredicate[U] =
		AnonymousElementPredicate( (matchState : ElementState[U]) => (this(matchState) || that(matchState) && !(this(matchState) && that(matchState))))
}

// maybe match and element predicates should be combined together into one trait

case class AnonymousMatchPredicate[-T](predicateFn : MatchState[T] => Boolean) extends MatchPredicate[T] {
	// type erasure
	//def this(predicateFn : MatchState[T] => Option[Boolean]) = this(MatchPredicateImplicits.matchPredicateOption2Boolean(predicateFn))

	def apply(matchState : MatchState[T]) : Boolean =
		predicateFn(matchState)
}

case class AnonymousElementPredicate[-T](predicateFn : ElementState[T] => Boolean) extends ElementPredicate[T] {
	// type erasure
	//def this(predicateFn : MatchState[T] => Option[Boolean]) = this(MatchPredicateImplicits.matchPredicateOption2Boolean(predicateFn))

	def apply(elementState : ElementState[T]) : Boolean =
		predicateFn(elementState)
}


trait MatchPredicateImplicits {
	implicit def matchPredicate2Anonymous[T](matchPredicate : MatchPredicate[T]) : AnonymousMatchPredicate[T] =
		AnonymousMatchPredicate( (matchState : MatchState[T]) => matchPredicate(matchState))

	implicit def elementPredicate2Anonymous[T](elementPredicate : ElementPredicate[T]) : AnonymousElementPredicate[T] =
		AnonymousElementPredicate( (elementState : ElementState[T]) => elementPredicate(elementState))

	implicit def matchPredicateOption2Boolean[T](predicateFn : MatchState[T] => Option[Boolean]) : MatchState[T] => Boolean =
		predicateFn.andThen {
			case Some(x) => x
			case _ => false
		}

	// TODO make this an implicit class?
	implicit def literal2Predicate[T](predicateFn : MatchState[T] => Boolean) : MatchPredicate[T] =
		AnonymousMatchPredicate(predicateFn)

	implicit def elementLiteral2Predicate[T](predicateFn : ElementState[T] => Boolean) : ElementPredicate[T] =
		AnonymousElementPredicate(predicateFn)

}

object MatchPredicate extends MatchPredicateImplicits {}