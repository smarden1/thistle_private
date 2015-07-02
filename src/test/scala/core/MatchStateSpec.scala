import org.scalatest.FunSpec
import core._

class MatchStateSpec extends FunSpec {

	def series : Vector[String] =
		Vector("a", "b", "c", "d", "e", "f")

	describe("previousMatchedValue") {
		it("should return the last matched value if there is one") {
			val ms = MatchState(List(1,2,3), 4)(series)
			assert(ms.previousMatchedValue == "d")
		}

		it("should throw errors if there are no matches") {
			intercept[NoSuchElementException] {
				MatchState(List(), 2)(series).previousMatchedValue
			}
		}
	}
}