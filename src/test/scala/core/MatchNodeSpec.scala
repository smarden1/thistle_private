package core

import org.scalatest.FunSpec

class MatchNodeSpec extends FunSpec {

	implicit val series : Vector[Char] =
		Vector('a', 'b', 'c', 'd', 'e', 'f')

	describe("MutableMatchNode") {
		it("should convert to an ImmutableMatchNode") {
			assert(
				MutableMatchNode(0, 0).toImmutableNode == ImmutableMatchNode(0, 0)
			)
		}
	}
	
}