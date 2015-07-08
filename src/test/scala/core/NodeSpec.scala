import org.scalatest.FunSpec
import core._

class NodeSpec extends FunSpec {

	/*
	 *	       a
  	 *	  b       e
	 *  c   d        f
	 */	
	def simpleTree() : SimpleNode = {
		val a = SimpleNode("a")
		val b = a.createAndAddChild("b")
		val c = b.createAndAddChild("c")
		val d = b.createAndAddChild("d")
		val e = a.createAndAddChild("e")
		val f = e.createAndAddChild("f")

		a
	}

	/*
	 *	           a
  	 *	    b       c         d
	 *    e   f        g    h   i   j
	 *  k
	 */	
	
	def complexTree() : SimpleNode = {
		val a = SimpleNode("a")
		val b = a.createAndAddChild("b")
		val c = a.createAndAddChild("c")
		val d = a.createAndAddChild("d")
		val e = b.createAndAddChild("e")
		val f = b.createAndAddChild("f")
		val g = c.createAndAddChild("g")
		val h = d.createAndAddChild("h")
		val i = d.createAndAddChild("i")
		val j = d.createAndAddChild("j")
		val k = e.createAndAddChild("k")

		a
	}

	def extractNodeLabels(collection : List[SimpleNode]) : List[String] =
		collection.map(_.label)

	describe("depthWalk") {
		it("should walk in order for simple tree") {
			assert(
				Node.depthWalk(simpleTree()).map(_.label).toList ==
				List("a", "b", "c", "d", "e", "f")
			)
		}
	}

	describe("breadthWalk") {
		it("should walk in order for a simple tree") {
			assert(
				Node.breadthWalk(simpleTree()).map(_.label).toList ==
				List("a", "b", "e", "c", "d", "f")
			)
		}
	}

	describe("prefixWalk") {
		it("should find all prefixs for a simple tree") {
			val a = SimpleNode("a")
			val b = a.createAndAddChild("b")
			val c = b.createAndAddChild("c")
			val d = b.createAndAddChild("d")
			val e = a.createAndAddChild("e")
			val f = e.createAndAddChild("f")

			assert(
				Node.prefixWalk(f).map(extractNodeLabels).toList ==
				List(List("f"))
			)

			assert(
				Node.prefixWalk(e).map(extractNodeLabels).toList ==
				List(List("e"), List("e", "f"))
			)

			assert(
				Node.prefixWalk(a).map(extractNodeLabels).toList ==
				List(
					List("a"),
					List("a", "b"),
					List("a", "b", "c"),
					List("a", "b", "d"),
					List("a", "e"),
					List("a", "e", "f")
				)
			)

			
		}
		it("should find all prefixs in complex tree") {
			assert(
				Node.prefixWalk(complexTree).map(extractNodeLabels).toList ==
				List(
					List("a"),
					List("a", "b"),
					List("a", "b", "e"),
					List("a", "b", "e", "k"),
					List("a", "b", "f"),
					List("a", "c"),
					List("a", "c", "g"),
					List("a", "d"),
					List("a", "d", "h"),
					List("a", "d", "i"),
					List("a", "d", "j")
				)
			)
		}
		it("should work with adding a child") {
			val node = new MutableMatchNode(0, 0)
			assert(Node.prefixWalk[MutableMatchNode](node).size == 1)
			node.createAndAddChild(2)
			assert(Node.prefixWalk[MutableMatchNode](node).size == 2)
		}
	}

	describe("subTreeWalk") {
		it("should find all subtrees for a simple tree") {
			val a = SimpleNode("a")
			val b = a.createAndAddChild("b")
			val c = b.createAndAddChild("c")
			val d = b.createAndAddChild("d")
			val e = a.createAndAddChild("e")
			val f = e.createAndAddChild("f")

			assert(
				Node.subTreeWalk(f).map(extractNodeLabels).toList ==
				List(List("f"))
			)

			assert(
				Node.subTreeWalk(e).map(extractNodeLabels).toList ==
				List(List("e", "f"))
			)

			assert(
				Node.subTreeWalk(a).map(extractNodeLabels).toList ==
				List(
					List("a", "b", "c"),
					List("a", "b", "d"),
					List("a", "e", "f")
				)
			)
		}

		it("should find all subtrees in complex tree") {
			assert(
				Node.subTreeWalk(complexTree).map(extractNodeLabels).toList ==
				List(
					List("a", "b", "e", "k"),
					List("a", "b", "f"),
					List("a", "c", "g"),
					List("a", "d", "h"),
					List("a", "d", "i"),
					List("a", "d", "j")
				)
			)
		}

		it("should find all subtrees in complex tree given a list of children") {
			assert(
				Node.subTreeWalk(complexTree.children).map(extractNodeLabels).toList ==
				List(
					List("b", "e", "k"),
					List("b", "f"),
					List("c", "g"),
					List("d", "h"),
					List("d", "i"),
					List("d", "j")
				)
			)
		}
	}

	describe("maxDepth") {
		it("should find the max depth for a simple tree") {
			val a = SimpleNode("a")
			val b = a.createAndAddChild("b")
			val c = b.createAndAddChild("c")
			val d = b.createAndAddChild("d")
			val e = a.createAndAddChild("e")
			val f = e.createAndAddChild("f")

			assert(Node.maxDepth(a) == 3)
			assert(Node.maxDepth(b) == 2)
		}

		it("should find the max depth for a complex tree") {
			assert(Node.maxDepth(complexTree) == 4)
		}

		it("should have depth of one for single nodes") {
			assert(Node.maxDepth(SimpleNode("a")) == 1)
		}

		it("should have depth of zero for empty stacks") {
			assert(Node.maxDepth(Nil) == 0)
		}
	}
	
}