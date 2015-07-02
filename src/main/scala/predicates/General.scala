package predicates

import core.{ElementState, ElementPredicate}

import scala.reflect.{ClassTag, classTag}

object General {

	val wild : ElementPredicate[Any] =
		(m: ElementState[Any]) => true

	/** you probably only want me for testing **/
	val missing = !wild

	def ofType[T: ClassTag]: ElementPredicate[T] =
		(m: ElementState[T]) => m.value.isInstanceOf[T]
		
}