package thistle.predicates

import thistle.core.ElementState
import org.scalatest.FunSpec

class GeneralSpec extends FunSpec {

	describe("ofType") {
		it("should return true if it is the correct type") {
			implicit val v = Vector("a", 12.0, '1')
			val pString = General.ofType[String]
			val pDouble = General.ofType[java.lang.Double]
			val pChar = General.ofType[java.lang.Character]
			
			assert(pString(ElementState(0)))
			assert(pDouble(ElementState(1)))
			assert(pChar(ElementState(2)))
		}

		it("should return false if it is the incorrect type") {
			implicit val v = Vector("a", 'b', 1)
			val pString = General.ofType[String]
			val pChar = General.ofType[Char]
			val pInt = General.ofType[Int]
			
			assert(!pString(ElementState(1)))
			assert(!pString(ElementState(2)))
			assert(!pChar(ElementState(0)))
			assert(!pInt(ElementState(1)))
		}
	}

	describe("equalsValue") {
		it("should return true if everything equals") {
			implicit val v = Vector("a", 'b', 1)
			val equalsA = General.equalsValue("a")
			val equalsB = General.equalsValue('b')
			val equalsOne = General.equalsValue(1)
			
			assert(equalsA(ElementState(0)))
			assert(equalsB(ElementState(1)))
			assert(equalsOne(ElementState(2)))
		}

		it("should return false if nothing equals") {
			implicit val v = Vector("a", 'b', 1)
			val equalsA = General.equalsValue('a')
			val equalsB = General.equalsValue('b')
			val equalsOne = General.equalsValue(1)
			
			assert(!equalsA(ElementState(0)))
			assert(!equalsA(ElementState(1)))
			assert(!equalsA(ElementState(2)))
			
			assert(!equalsB(ElementState(0)))
			assert(!equalsB(ElementState(2)))

			assert(!equalsOne(ElementState(0)))
			assert(!equalsOne(ElementState(1)))
		}
	}
}