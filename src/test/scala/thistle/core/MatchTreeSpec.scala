package thistle.core

import org.scalatest.FunSpec

import thistle.predicates.{General, Character}

class MatchTreeSpec extends FunSpec {
  implicit val series: Vector[Char] =
    Vector('a', 'b', 'c', 'd', 'e', 'f')

  describe("all") {
    it("should return all matches for a query of size 1 that has a single match") {
      val query = Query(Character.isCharacter('b'))
      val mt = MatchTree(query)
      val node = ImmutableMatchNode(1, 0, Nil)

      assert(mt.allMatches.size == 1)
      assert(mt.allValues.toList == List(List('b')))
      assert(mt.allIndexes.toList == List(List(1)))
      assert(mt.allMatches.toList == List(Match(List(node), query)))
      assert(mt.allCompleteMatches.toList == List(Match(List(node), query)))
      assert(mt.allIncompleteMatches.isEmpty)
    }

    it("should return no matches for a query of size 1 that doesn't match") {
      val query = Query(Character.isCharacter('q'))
      val mt = MatchTree(query)

      assert(mt.allMatches.isEmpty)
      assert(mt.allValues.isEmpty)
      assert(mt.allIndexes.isEmpty)
      assert(mt.allMatches.isEmpty)
      assert(mt.allCompleteMatches.isEmpty)
      assert(mt.allIncompleteMatches.isEmpty)
    }

    it("should return all matches for a query of size 2 that has a single match") {
      val query = Query(Character.isCharacter('b'), General.equalsValue('c'))
      val mt = MatchTree(query)
      val cNode = ImmutableMatchNode(2, 1, Nil)
      val bNode = ImmutableMatchNode(1, 0, List(cNode))

      assert(mt.allMatches.size == 1)
      assert(mt.allValues.toList == List(List('b', 'c')))
      assert(mt.allIndexes.toList == List(List(1, 2)))
      assert(mt.allMatches.toList == List(Match(List(bNode, cNode), query)))
      assert(mt.allCompleteMatches.toList == List(Match(List(bNode, cNode), query)))
      assert(mt.allIncompleteMatches.isEmpty)
    }

    it("should return all matches for a query of size 3 that has a single match") {
      val query = Query(Character.isCharacter('b'), General.equalsValue('c'), General.equalsValue('d'))
      val mt = MatchTree(query)
      val dNode = ImmutableMatchNode(3, 2, Nil)
      val cNode = ImmutableMatchNode(2, 1, List(dNode))
      val bNode = ImmutableMatchNode(1, 0, List(cNode))

      assert(mt.allMatches.size == 1)
      assert(mt.allValues.toList == List(List('b', 'c', 'd')))
      assert(mt.allIndexes.toList == List(List(1, 2, 3)))
      assert(mt.allMatches.toList == List(Match(List(bNode, cNode, dNode), query)))
      assert(mt.allCompleteMatches.toList == List(Match(List(bNode, cNode, dNode), query)))
      assert(mt.allIncompleteMatches.isEmpty)
    }

    it("should return all matches for a query that is incomplete") {
      val query = Query(Character.isCharacter('b'), General.equalsValue('c'), General.equalsValue('k') )
      val mt = MatchTree(query)
      val cNode = ImmutableMatchNode(2, 1, Nil)
      val bNode = ImmutableMatchNode(1, 0, List(cNode))

      assert(mt.allMatches.size == 1)
      assert(mt.allValues.toList == List(List('b', 'c')))
      assert(mt.allIndexes.toList == List(List(1, 2)))
      assert(mt.allMatches.toList == List(Match(List(bNode, cNode), query)))
      assert(mt.allCompleteMatches.isEmpty)
      assert(mt.allIncompleteMatches.toList == List(Match(List(bNode, cNode), query)))
    }

    it("should return all matches for a query that has multiple branches") {
      implicit val series = Vector('a', 'b', 'b', 'c', 'b')
      val query = Query(Character.isCharacter('a'), General.equalsValue('b'), General.equalsValue('c') )
      val mt = MatchTree(query)
      val cNode = ImmutableMatchNode(3, 2, Nil)
      val firstBNode = ImmutableMatchNode(1, 1, List(cNode))
      val secondBNode = ImmutableMatchNode(2, 1, List(cNode))
      val thirdBNode = ImmutableMatchNode(4, 1, Nil)
      val aNode = ImmutableMatchNode(0, 0, List(firstBNode, secondBNode, thirdBNode))
      val match1 = Match(List(aNode, firstBNode, cNode), query)
      val match2 = Match(List(aNode, secondBNode, cNode), query)
      val match3 = Match(List(aNode, thirdBNode), query)

      assert(mt.allMatches.size == 3)
      assert(mt.allValues.toList == List(List('a', 'b', 'c'), List('a', 'b', 'c'), List('a', 'b')))
      assert(mt.allIndexes.toList == List(List(0, 1, 3), List(0, 2, 3), List(0, 4)))
      assert(mt.allMatches.toList == List(match1, match2, match3))
      assert(mt.allCompleteMatches.toList == List(match1, match2))
      assert(mt.allIncompleteMatches.toList == List(match3))
    }
  }

  describe("uniqueCountsPerStep") {
    it("should count unique counts") {
      implicit val series = Vector('a', 'b', 'b', 'c', 'b')
      val query = Query(Character.isCharacter('a'), General.equalsValue('b'), General.equalsValue('c') )
      val mt = MatchTree(query)

      assert(mt.uniqueCountsPerStep == List(1, 3, 1))
    }
  }
}