import org.scalatest.FunSpec
import core._

class ElementSpec extends FunSpec {

	def series : Vector[String] =
		Vector("a", "b", "c", "d", "e", "f")

	describe("isHead") {
		it("should return true if it is the head of a series") {
			assert(Element(0)(series).isHead)
		}

		it("should return false if it is not the head of a series") {
			assert(!Element(2)(series).isHead)
		}
	}

	describe("isLast") {
		it("should return true if it is the last in a series") {
			assert(Element(5)(series).isLast)
		}

		it("should return false if it is not the last of a series") {
			assert(!Element(3)(series).isLast)
		}
	}

	describe("nextElement") {
		it("should return the next sequential Option[element] if there is one") {
			assert(Element(0)(series).nextElement.get == Element(1)(series))
		}

		it("should return None if there is no next element") {
			assert(Element(5)(series).nextElement.isEmpty)
		}
	}

	describe("nextValue") {
		it("should return the next sequential Option[value] if there is one") {
			assert(Element(0)(series).nextValue.get == "b")
		}

		it("should return None if there is no next element") {
			assert(Element(5)(series).nextValue.isEmpty)
		}
	}

	describe("constructor") {
		it("should fail if the elementIndex is larger than the series length") {
			intercept[IllegalArgumentException] {
				Element(10)(series)
			}
		}

		it("should fail if the elementIndex is a negative number") {
			intercept[IllegalArgumentException] {
				Element(-1)(series)
			}
		}
	}

	describe("previousValues") {
		it("should return the previous values") {
			assert(Element(2)(series).previousValues == List("a", "b"))
		}

		it("should return Nil if there are no previous values") {
			assert(Element(0)(series).previousValues == Nil)
		}
	}

	describe("nextValues") {
		it("should return the next values and not include the current value") {
			assert(Element(2)(series).nextValues == List("d", "e", "f"))

			assert(Element(4)(series).nextValues == List("f"))
		}

		it("should return Nil if there are no next values") {
			assert(Element(5)(series).nextValues == Nil)
		}
	}

	describe("isNthInSeries") {
		it("should return true if it is the nth in the series") {
			assert(Element(0)(series).isNthInSeries(0))
			assert(Element(1)(series).isNthInSeries(1))
		}

		it("should return false if it is not the nth in the series") {
			assert(!Element(0)(series).isNthInSeries(1))
			assert(!Element(1)(series).isNthInSeries(0))
			assert(!Element(3)(series).isNthInSeries(5))
			assert(!Element(5)(series).isNthInSeries(3))
		}
	}
}