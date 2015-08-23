package core

import org.scalatest.FunSpec

class MatchStateSpec extends FunSpec {

	implicit val series: Vector[String] =
		Vector("a", "b", "c", "d", "e", "f")

	describe("previousMatchedValue") {
		it("should return the last matched value if there is one") {
			val ms = MatchState(List(1, 2, 3), 4)

			assert(ms.previousMatchedValue == "d")
		}

		it("should throw errors if there are no matches") {
			intercept[NoSuchElementException] {
				MatchState(List(), 2).previousMatchedValue
			}
		}
	}
}