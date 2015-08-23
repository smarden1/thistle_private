package core

import org.scalatest.FunSpec
import scala.reflect.runtime.universe._

import predicates.General._
import predicates.Character._

class PredicateSpec extends FunSpec {

	implicit val series: Vector[Char] =
		Vector('a', 'b', 'c', 'd', 'e', 'f')

	val charWild: MatchPredicate[Char] =
		(m: MatchState[Char]) => true

	val stringWild: MatchPredicate[String] =
		(m: MatchState[String]) => true

	val elementStringWild: MatchPredicate[String] =
		(m: ElementState[String]) => true

	def predicateTypesMatch[T: TypeTag, U: TypeTag](first: T, second: U): Boolean =
  		typeOf[T].typeArgs == typeOf[U].typeArgs

	describe("combining predicates") {
		it("should be able to combine predicates of the same type") {
			val twoWilds = wild && wild
			val ms = MatchState(List(0), 1)
			
			assert(twoWilds(ms) == wild(ms))
		}

		it("should be able to combine predicates of a different type") {
			val twoWilds = wild && charWild
			val ms = MatchState(List(0), 1)
			
			assert(twoWilds(ms) == wild(ms))
		}

		it("should be able to combine matchState and elementState predicates") {
			val twoWilds = stringWild && elementStringWild
			val ms = MatchState(List(0), 1)(series.map(_.toString))
			
			assert(twoWilds(ms) == wild(ms))
			assert(elementStringWild(ms) == wild(ms))
		}

		it("should be able to combine predicates of a different type and become the correct type") {
			assert(predicateTypesMatch(wild && wild, wild))
			assert(predicateTypesMatch(wild && charWild, charWild))
			assert(predicateTypesMatch(wild && charWild, charWild && wild))
		}

		it("should combine predicates and pick the lowest common denominator type") {
			//assert(predicateTypesMatch(wild && charWild && stringWild, charWild))
		}
	}
}