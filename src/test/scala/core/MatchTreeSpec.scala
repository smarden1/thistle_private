package core

import org.scalatest.FunSpec

import predicates.General

class MatchTreeSpec extends FunSpec {
	implicit val series : Vector[Char] =
		Vector('a', 'b', 'c', 'd', 'e', 'f')

	describe("findAll") {
		it("should return all matches for a query of size 1 that has a single match") {
			val query = Query(General.equals('b'))
			val mt = MatchTree(query)
			val node = ImmutableMatchNode(1, 0, Nil)

			assert(mt.findAllMatches.size == 1)
			assert(mt.findAllValues.toList == List(List('b')))
			assert(mt.findAllIndexes.toList == List(List(1)))
			assert(mt.findAllMatches.toList == List(Match(List(node), query)))
			assert(mt.findAllCompleteMatches.toList == List(Match(List(node), query)))
			assert(mt.findAllIncompleteMatches.isEmpty)
		}

		it("should return no matches for a query of size 1 that doesn't match") {
			val query = Query(General.equals('q'))
			val mt = MatchTree(query)

			assert(mt.findAllMatches.isEmpty)
			assert(mt.findAllValues.isEmpty)
			assert(mt.findAllIndexes.isEmpty)
			assert(mt.findAllMatches.isEmpty)
			assert(mt.findAllCompleteMatches.isEmpty)
			assert(mt.findAllIncompleteMatches.isEmpty)
		}

		it("should return all matches for a query of size 2 that has a single match") {
			val query = Query(General.equals('b'), General.equals('c'))
			val mt = MatchTree(query)
			val cNode = ImmutableMatchNode(2, 1, Nil)
			val bNode = ImmutableMatchNode(1, 0, List(cNode))

			assert(mt.findAllMatches.size == 1)
			assert(mt.findAllValues.toList == List(List('b', 'c')))
			assert(mt.findAllIndexes.toList == List(List(1, 2)))
			assert(mt.findAllMatches.toList == List(Match(List(bNode, cNode), query)))
			assert(mt.findAllCompleteMatches.toList == List(Match(List(bNode, cNode), query)))
			assert(mt.findAllIncompleteMatches.isEmpty)
		}

		it("should return all matches for a query of size 3 that has a single match") {
			val query = Query(General.equals('b'), General.equals('c'), General.equals('d'))
			val mt = MatchTree(query)
			val dNode = ImmutableMatchNode(3, 2, Nil)
			val cNode = ImmutableMatchNode(2, 1, List(dNode))
			val bNode = ImmutableMatchNode(1, 0, List(cNode))

			assert(mt.findAllMatches.size == 1)
			assert(mt.findAllValues.toList == List(List('b', 'c', 'd')))
			assert(mt.findAllIndexes.toList == List(List(1, 2, 3)))
			assert(mt.findAllMatches.toList == List(Match(List(bNode, cNode, dNode), query)))
			assert(mt.findAllCompleteMatches.toList == List(Match(List(bNode, cNode, dNode), query)))
			assert(mt.findAllIncompleteMatches.isEmpty)
		}

		it("should return all matches for a query that is incomplete") {
			val query = Query(General.equals('b'), General.equals('c'), General.equals('k') )
			val mt = MatchTree(query)
			val cNode = ImmutableMatchNode(2, 1, Nil)
			val bNode = ImmutableMatchNode(1, 0, List(cNode))

			assert(mt.findAllMatches.size == 1)
			assert(mt.findAllValues.toList == List(List('b', 'c')))
			assert(mt.findAllIndexes.toList == List(List(1, 2)))
			assert(mt.findAllMatches.toList == List(Match(List(bNode, cNode), query)))
			assert(mt.findAllCompleteMatches.isEmpty)
			assert(mt.findAllIncompleteMatches.toList == List(Match(List(bNode, cNode), query)))
		}

		it("should return all matches for a query that has multiple branches") {
			implicit val series = Vector('a', 'b', 'b', 'c', 'b')
			val query = Query(General.equals('a'), General.equals('b'), General.equals('c') )
			val mt = MatchTree(query)
			val cNode = ImmutableMatchNode(3, 2, Nil)
			val firstBNode = ImmutableMatchNode(1, 1, List(cNode))
			val secondBNode = ImmutableMatchNode(2, 1, List(cNode))
			val thirdBNode = ImmutableMatchNode(4, 1, Nil)
			val aNode = ImmutableMatchNode(0, 0, List(firstBNode, secondBNode, thirdBNode))
			val match1 = Match(List(aNode, firstBNode, cNode), query)
			val match2 = Match(List(aNode, secondBNode, cNode), query)
			val match3 = Match(List(aNode, thirdBNode), query)

			assert(mt.findAllMatches.size == 3)
			assert(mt.findAllValues.toList == List(List('a', 'b', 'c'), List('a', 'b', 'c'), List('a', 'b')))
			assert(mt.findAllIndexes.toList == List(List(0, 1, 3), List(0, 2, 3), List(0, 4)))
			assert(mt.findAllMatches.toList == List(match1, match2, match3))
			assert(mt.findAllCompleteMatches.toList == List(match1, match2))
			assert(mt.findAllIncompleteMatches.toList == List(match3))
		}
	}
}