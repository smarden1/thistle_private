package core

import org.scalatest.FunSpec

import predicates.{Indexes, General}

class MatchSequenceSpec extends FunSpec with MatchSequenceImplicits {

	def example: Seq[Int] =
		0 to 10

	describe("filter") {
		it("should return all filtered values") {
			val s = (0 to 10)
			val v = s.toVector

			assert(s.filter(Indexes.even) == Seq(0, 2, 4, 6, 8, 10))
			assert(v.filter(Indexes.even) == Vector(0, 2, 4, 6, 8, 10))
		}
	}

	describe("forall") {
		it("should return true if the predicate is always true") {
			assert(example.forall(General.wild))
		}

		it("should return false if the predicate is not always true") {
			assert(!example.forall(Indexes.even))
		}
	}

	describe("exists") {
		it("elementPredicate: should return true if the predicate is true at least once") {
			assert(example.exists(Indexes.odd))
		}

		it("elementPredicate: should return false if the predicate is never true") {
			assert(!example.exists(!General.wild))
		}

		it("query: should return true if there is at least one completed predicate") {
			val q = Query(Indexes.odd, Indexes.odd)
			assert(example.exists(q))
		}
	}

	describe("find") {
		it("should find the first item that satisfies the predicate") {
			assert(example.find(Indexes.atIndex(4)) == Some(4))
		}

		it("should return None if no item that satisfies a predicate") {
			assert(example.find(Indexes.atIndex(14)) == None)
		}
	}
}