package predicates

import core.{ElementState, ElementPredicate, AnonymousElementPredicate}

import scala.reflect.{ClassTag, classTag}

import scala.reflect.runtime.universe._

object General {

	val wild : ElementPredicate[Any] =
		(m: ElementState[Any]) => true

	val missing = !wild

	def ofType[T: TypeTag] =
		_ofType[T, Any]

	def _ofType[T: TypeTag, U: ClassTag] : ElementPredicate[U] =
		(m: ElementState[U]) => {
			//classTag[T].runtimeClass.isInstance(m.value)
			//classTag[U] <:< classTag[T]
			//typeOf[T] =:= typeOf[U]
			val mirror = runtimeMirror(getClass.getClassLoader)
			//val tm = typeTag[T].tpe
			//println(mirror.reflect(m.value).symbol)
			//println(typeOf[T].typeSymbol.asClass)

			// try this one
			//println(classTag[T].runtimeClass.asInstanceOf[Class[T]])
			//
			println(mirror.reflect(m.value).symbol.toType.asInstanceOf[TypeRefApi])
			println(typeOf[T].typeSymbol.asClass.toType.asInstanceOf[TypeRefApi])
			//println(tm.typeSymbol)
			//mirror.reflect(m.value).symbol.toType =:= typeOf[T].typeSymbol.asClass.toType
			//mirror.reflect(m.value).symbol.toType.asInstanceOf[TypeRefApi] == typeOf[T].typeSymbol.asClass.toType.asInstanceOf[TypeRefApi]
			mirror.reflect(m.value).symbol.toType =:= typeOf[T].typeSymbol.asClass.toType

			// TODO - can we recongize if a primitive is java or scala and convertit?

			//m.value.isInstanceOf[T]
		}

	//def ofType[T] = {
		//def _ofType[T, U >: T](implicit tag: TypeTag[U], ctag: ClassTag[T]) : ElementPredicate[U] =
			//(m: ElementState[U]) => {
				//classTag[T].runtimeClass.isInstance(m.value)
			//}
		//_ofType[T, Any]
	//}

	def getTypeTag[T: TypeTag](obj: T) =
		typeTag[T]

	def getClassTag[T: ClassTag](obj: T) =
		classTag[T]
		

	// any?
	def equals[T](value : T): ElementPredicate[T] =
		(m: ElementState[T]) => m.value == value
}