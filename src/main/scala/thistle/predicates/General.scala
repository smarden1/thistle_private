package thistle.predicates

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._

import thistle.core.{ElementState, ElementPredicate, AnonymousElementPredicate}

object General {

	val wild: ElementPredicate[Any] =
		(m: ElementState[Any]) => true

	val missing =
		!wild

	def ofType[T: TypeTag] = {
		def _ofType[T: TypeTag, U: ClassTag]: ElementPredicate[U] =
			(m: ElementState[U]) => {
				val mirror = runtimeMirror(getClass.getClassLoader)
				mirror.reflect(m.value).symbol.toType =:= typeOf[T].typeSymbol.asClass.toType
		}

		_ofType[T, Any]
	}

	def equalsValue(value: Any): ElementPredicate[Any] =
		(m: ElementState[Any]) => m.value == value
}