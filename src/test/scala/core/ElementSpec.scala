import org.scalatest.FunSpec
import core._

class ElementSpec extends FunSpec {

	implicit val series : Vector[String] =
		Vector("a", "b", "c", "d", "e", "f")

	describe("isHead") {
		it("should return true if it is the head of a series") {
			assert(Element(0).isHead)
		}

		it("should return false if it is not the head of a series") {
			assert(!Element(2).isHead)
		}
	}

	describe("isLast") {
		it("should return true if it is the last in a series") {
			assert(Element(5).isLast)
		}

		it("should return false if it is not the last of a series") {
			assert(!Element(3).isLast)
		}
	}

	describe("nextElement") {
		it("should return the next sequential Option[element] if there is one") {
			assert(Element(0).nextElement.get == Element(1))
		}

		it("should return None if there is no next element") {
			assert(Element(5).nextElement.isEmpty)
		}
	}

	describe("nextValue") {
		it("should return the next sequential Option[value] if there is one") {
			assert(Element(0).nextValue.get == "b")
		}

		it("should return None if there is no next element") {
			assert(Element(5).nextValue.isEmpty)
		}
	}

	describe("constructor") {
		it("should fail if the elementIndex is larger than the series length") {
			intercept[IllegalArgumentException] {
				Element(10)
			}
		}

		it("should fail if the elementIndex is a negative number") {
			intercept[IllegalArgumentException] {
				Element(-1)
			}
		}
	}

	describe("previousValues") {
		it("should return the previous values") {
			assert(Element(2).previousValues == List("a", "b"))
		}

		it("should return Nil if there are no previous values") {
			assert(Element(0).previousValues == Nil)
		}
	}

	describe("nextValues") {
		it("should return the next values and not include the current value") {
			assert(Element(2).nextValues == List("d", "e", "f"))

			assert(Element(4).nextValues == List("f"))
		}

		it("should return Nil if there are no next values") {
			assert(Element(5).nextValues == Nil)
		}
	}

	describe("isNthInSeries") {
		it("should return true if it is the nth in the series") {
			assert(Element(0).isNthInSeries(0))
			assert(Element(1).isNthInSeries(1))
		}

		it("should return false if it is not the nth in the series") {
			assert(!Element(0).isNthInSeries(1))
			assert(!Element(1).isNthInSeries(0))
			assert(!Element(3).isNthInSeries(5))
			assert(!Element(5).isNthInSeries(3))
		}
	}
}