package collection

// All methods O(1)
case object EmptyTree extends BinaryTree {
  override val count = 0
  override def deep = 0
  override def nth(i: Int) = throw new Exception

  override def add(a: Int) = NonEmptyTree(a)
  override def add(a: Int, re: Int) = NonEmptyTree(a, EmptyTree, EmptyTree, re)

  override def remove(a: Int) = throw new Exception("Can't remove element: this collection is empty")
  override def remove(a: Int, re: Int) = throw new Exception("Can't remove elements: this collection is empty")

  override def toList = List[Int]()
  override def toString = "."
}