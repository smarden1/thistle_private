package core

case class SimpleNode(label : String) extends MutableNode[SimpleNode]() {
	def createAndAddChild(label : String) : SimpleNode = {
		val node = SimpleNode(label)
		addChild(node)

		node
	}

	def iterator() : Iterator[SimpleNode] =
		Node.depthWalk(this)

	override def toString() : String =
		"SimpleNode(%s)".format(label)
}