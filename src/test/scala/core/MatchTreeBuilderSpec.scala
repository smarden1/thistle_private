package core

import org.scalatest.FunSpec

import core._
import predicates.General._
import predicates.Indexes

import predicates.Character._

class MatchTreeBuilderSpec extends FunSpec {
	implicit val series : Vector[Char] =
		Vector('a', 'b', 'c', 'd', 'e', 'f')

	def numericSeries : Vector[Int] =
		(0 to 10).toVector

	describe("isValidMatch") {
		it("should return true for a simple valid match") {
			
			val ms = MatchState(List(), 1)
			val mt = new MatchTreeBuilder(Query(wild))
			assert(mt.isValidMatch(ms))

			val ms2 = MatchState(List(), 1)(numericSeries)
			val mt2 = new MatchTreeBuilder(Query(Indexes.odd))(numericSeries)
			assert(mt2.isValidMatch(ms2))
		}

		it("should return true for a simple valid match regardless if there are previous matches") {
			val mt = new MatchTreeBuilder(Query(wild, wild))
			val ms = MatchState(List(0), 1)

			assert(mt.isValidMatch(ms))
		}

		it("should return false for a simple valid match if all predicates have been exhausted") {
			val mt = new MatchTreeBuilder(Query(wild, wild))
			val ms = MatchState(List(0, 1), 1)

			assert(!mt.isValidMatch(ms))
		}

		it("should return false for a simple valid match") {
			val ms = MatchState(List(), 1)
			val mt = new MatchTreeBuilder(Query(!wild))
			
			assert(!mt.isValidMatch(ms))
		}

		it("should return false for a simple valid match regardless of previous matches") {
			
			val ms = MatchState(List(0), 1)
			val mt = new MatchTreeBuilder(Query(!wild))
			
			assert(!mt.isValidMatch(ms))
		}
	}
}