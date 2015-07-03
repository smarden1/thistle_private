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
				List(List("e"), List("f", "e"))
			)

			assert(
				Node.prefixWalk(a).map(extractNodeLabels).toList ==
				List(
					List("a"),
					List("b", "a"),
					List("c", "b", "a"),
					List("d", "b", "a"),
					List("e", "a"),
					List("f", "e", "a")
				)
			)

			
		}
		it("should find all prefixs in complex tree") {
			assert(
				Node.prefixWalk(complexTree).map(extractNodeLabels).toList ==
				List(
					List("a"),
					List("b", "a"),
					List("e", "b", "a"),
					List("k", "e", "b", "a"),
					List("f", "b", "a"),
					List("c", "a"),
					List("g", "c", "a"),
					List("d", "a"),
					List("h", "d", "a"),
					List("i", "d", "a"),
					List("j", "d", "a")
				)
			)
		}
		it("should work on a root") {
			val node = new MutableMatchNode(0, 0) with Root[MutableMatchNode]
			val node2 = new MutableMatchNode(0, 0)
			assert(Node.prefixWalk[MutableMatchNode](node).size == 1) // seq has iterator and itertor doesnt include
			node.createAndAddChild(2)
			assert(Node.prefixWalk[MutableMatchNode](node).size == 2)
			assert(Node.prefixWalk[MutableMatchNode](node2).size == 1)
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