package thistle.examples.webevent

import org.scalatest.FunSpec

import thistle.core.{MatchSequence, MatchTree}
import thistle.predicates.General._
import thistle.core.{Query, Node}
import thistle.examples.webevent.Predicates._

class QueriesTestSpec extends FunSpec {

	def generateEvent(ref: String, url: String): SearchEvent =
		SearchEvent(
			ref,
			url,
			1,
			"query",
			Seq(1,2,3)
		)

	describe("PurchaseChannel") {
		it("should find a single purchase channel") {
			val visit = Vector(
				SearchEvent(
					"",
					"searchpage1",
					1,
					"something",
					List(1,2,3,4,5)
				),
				ListingEvent(
					"searchpage1",
					"listing_page1",
					2,
					4,
					99
				),
				PurchaseEvent(
					"listing_page1",
					"purchase",
					3,
					List(3,4)
				)
			)

			val Test = Query(
				ofType[WebEvent] || ofType[SearchEvent], samePage
			)

			MatchTree(Test)(visit)
		}
	}

	describe("tabbedBrowsing") {
		it("should generate the correct sized query") {
			assert(Queries.tabbedBrowsing(5).tail.size == 4)
		}

		it("should find no tabs when there are none") {
			val visit = Vector(
				generateEvent("", "zero"),
				generateEvent("zero", "first"),
				generateEvent("first", "second")
			)

			val expectedIndexes = Set(
				Seq(0, 1, 2)
			)

			val mt = MatchTree(Queries.tabbedBrowsing(visit.size))(visit)

			assert(mt.findAllCompleteMatches.size == 1)
			assert(mt.findAllIndexes.toSet == expectedIndexes)
		}

		it("should find a single tab") {
			val visit = Vector(
				generateEvent("", "zero"),
				generateEvent("zero", "first"),
				generateEvent("first", "second"),
				generateEvent("first", "third")
			)

			val expectedIndexes = Set(
				Seq(0, 1, 2),
				Seq(0, 1, 3)
			)

			val mt = MatchTree(Queries.tabbedBrowsing(visit.size))(visit)

			assert(mt.findAllCompleteMatches.isEmpty)
			assert(mt.findAllIndexes.toSet == expectedIndexes)
		}

		it("should find tabs when they existss") {
			/*
			 *
			 *           0
			 *        1      5
			 *     2     3
			 *  4         
			 */
			val visit = Vector(
				generateEvent("", "zero"),
				generateEvent("zero", "first"),
				generateEvent("first", "second"),
				generateEvent("first", "third"),
				generateEvent("second", "fourth"),
				generateEvent("zero", "fifth")
			)

			val expectedIndexes = Set(
				Seq(0, 1, 2, 4),
				Seq(0, 1, 3),
				Seq(0, 5)
			)

			val mt = MatchTree(Queries.tabbedBrowsing(visit.size))(visit)
			
			assert(mt.maxDepth == 4)
			assert(mt.findAllCompleteMatches.isEmpty)
			assert(mt.findAllIndexes.toSet == expectedIndexes)
		}

		it("should find tabs when they exist") {
			/*
			 *
			 *           0
			 *        1      6
			 *     2     3
			 *  4   5      7
			 */
			val visit = Vector(
				generateEvent("", "zero"),
				generateEvent("zero", "first"),
				generateEvent("first", "second"),
				generateEvent("first", "third"),
				generateEvent("second", "fourth"),
				generateEvent("second", "fifth"),
				generateEvent("zero", "sixth"),
				generateEvent("third", "seventh")
			)

			val expectedIndexes = Set(
				Seq(0, 1, 2, 4),
				Seq(0, 1, 2, 5),
				Seq(0, 1, 3, 7),
				Seq(0, 6)
			)

			val mt = MatchTree(Queries.tabbedBrowsing(visit.size))(visit)
			assert(mt.maxDepth == 4)
			assert(mt.findAllCompleteMatches.isEmpty)
			assert(mt.findAllIndexes.toSet == expectedIndexes)
		}
	}
}